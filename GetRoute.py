'''
Author: Zheyan Liu
Date:11/18/2021
'''

# %% packages
import pandas as pd
import googlemaps
import datetime
import warnings
warnings.filterwarnings("ignore")

# %% read data
# stations_df = pd.read_csv('data/subway_info_final3.csv', encoding= 'unicode_escape')
# target_stations_df = stations_df[stations_df['linename'].str.contains('A')]


stations_df = pd.read_csv('data/NYC_Transit_Subway_Entrance_And_Exit_Data.csv', encoding= 'unicode_escape')
stations_df = stations_df.fillna('')
stations_df['linename'] = stations_df['Route1'].astype('str') + stations_df['Route2'].astype('str') + stations_df['Route3'].astype('str') +  stations_df['Route4'].astype('str') +  stations_df['Route5'].astype('str') + stations_df['Route6'].astype('str')  + stations_df['Route7'].astype('str') + stations_df['Route8'].astype('str') + stations_df['Route9'].astype('str') +  stations_df['Route10'].astype('str') +  stations_df['Route11'].astype('str')
stations_df = stations_df[['Station Name', 'Station Latitude', 'Station Longitude', 'linename']]
stations_df.columns = ['station', 'lat', 'long', 'linename']
stations_df = stations_df.drop_duplicates()


# %%

def extract_info_from_direction(legs, route_num):

        step_dict = {'time':0,
                    'distance':0,
                    'walking_distance':0,
                    'subway_route':{}
        }

        # get general info: time & distance
        step_dict['time'] = legs['duration']['value'] # seconds
        step_dict['distance'] = legs['distance']['value'] # miles

        # subway setp cnt
        transit_num = 1

        # each step
        for step in legs['steps']:

            # process walking
            if step['travel_mode'] == 'WALKING':
                # add up walking distance
                step_dict['walking_distance'] += step['distance']['value'] # meters

            # process subway
            if step['travel_mode'] == 'TRANSIT':
                # print(step['transit_details'].keys())
                if step['transit_details']['line']['vehicle']['type'] == 'SUBWAY':
                    subway_route_dict = {}
                    # icon url: '//maps.gstatic.com/mapfiles/transit/iw2/6/us-ny-mta/A.png'
                    subway_route_dict['line'] = step['transit_details']['line']['short_name']
                    subway_route_dict['departure_stop'] = step['transit_details']['departure_stop']['name']
                    subway_route_dict['arrival_stop'] = step['transit_details']['arrival_stop']['name']
                    subway_route_dict['num_stops'] = step['transit_details']['num_stops']
                    # get departure and arrival time, can calculate time on the train
                    subway_route_dict['departure_time'] = step['transit_details']['departure_time']['value']
                    subway_route_dict['arrival_time'] = step['transit_details']['arrival_time']['value']
                    # get lat and lng
                    subway_route_dict['departure_stop_lat'] = step['transit_details']['departure_stop']['location']['lat']
                    subway_route_dict['departure_stop_lng'] = step['transit_details']['departure_stop']['location']['lng']
                    subway_route_dict['arrival_stop_lat'] = step['transit_details']['arrival_stop']['location']['lat']
                    subway_route_dict['arrival_stop_lng'] = step['transit_details']['arrival_stop']['location']['lng']

                    # append that in step_dict key 'subway_route'
                    step_dict['subway_route'][transit_num] = subway_route_dict
                    transit_num += 1
            

        # generate dataframe    
        subway_route = step_dict['subway_route']
        subway_route_df = pd.DataFrame.from_dict(subway_route).T.reset_index(drop = True)
        

        step_dict.pop('subway_route')
        other_info_df = pd.DataFrame.from_dict([step_dict]).reset_index(drop = True)
        # step_df = other_info_df.merge(subway_route_df, how='outer', left_on = 'time', right_on = 'num_stops')
        step_df = pd.concat([other_info_df, subway_route_df], axis = 1)
        step_df = step_df.fillna(step_df.mean())
        step_df['route_num'] = route_num

        return step_df


class google_routes():

    def __init__(self):
        self.key = 'AIzaSyAGrKCa5wYrYmkhiFQcKQ27oz0_jOivtkE'
        self.start_location = '168 St, New York, NY 10032'
        self.destination = 'Prospect Park, NY'
        self.departure_time = datetime.datetime.now()
        self.direction_df_list = []
        self.directions_df = pd.DataFrame()
        self.stations_df = stations_df


    def get_directions(self):
        gmaps = googlemaps.Client(key = self.key)
        directions = gmaps.directions(self.start_location, self.destination, mode = 'transit', 
                              transit_mode = 'subway', alternatives = True,  departure_time = self.departure_time)

        for route_num, direction in enumerate(directions):
            direction_df = extract_info_from_direction(direction['legs'][0], route_num + 1)
            self.direction_df_list.append(direction_df)
        
        if self.direction_df_list:
            self.directions_df = pd.concat(self.direction_df_list, ignore_index=True)
        else:
            self.directions_df = pd.DataFrame(0)
        # directions_df.to_csv('directions.csv', index = False)

        return self.directions_df

    def get_stops(self):

        stops_dfs = []
        for i in range(self.directions_df.shape[0]):
            directions_df = self.directions_df.iloc[[i],].reset_index(drop = True)
            # print(directions_df)
            route_num = directions_df['route_num'][0]
            line = directions_df['line'][0]
            num_stops = directions_df['num_stops'][0]
            target_stations_df = self.stations_df[self.stations_df['linename'].str.contains(line[0])]
            
            stops_dict = {}
            stop_info_dict = {'line':line}
            current_lat = directions_df['departure_stop_lat'][0]
            current_lng = directions_df['departure_stop_lng'][0]

            end_lat = directions_df['arrival_stop_lat'][0]
            end_lng = directions_df['arrival_stop_lng'][0]

            total_distance = (current_lat - end_lat)**2 + (current_lng - end_lng)**2

            for stop_num in range(num_stops+1):
                stop_info_dict =  {'line':line}
                # print(stop_num)
                # the start location
                if stop_num == 0:
                    stop_info_dict['station'] = directions_df['departure_stop'][0]
                    stop_info_dict['station_type'] = 'start'
                    stop_info_dict['lat'] = directions_df['departure_stop_lat'][0]
                    stop_info_dict['long'] = directions_df['departure_stop_lng'][0]
                
                # the last station
                elif stop_num == num_stops:
                    stop_info_dict['station'] = directions_df['arrival_stop'][0]
                    stop_info_dict['station_type'] = 'end'
                    stop_info_dict['lat'] = end_lat
                    stop_info_dict['long'] = end_lng
            
                # other stations in the middle, needs some calculation
                else:
                    
                    # candidates must make the distance shorter
                    target_stations_df['distance'] = (current_lat - target_stations_df['lat'])**2 + (current_lng - target_stations_df['long'])**2 
                    target_stations_df['distance_d'] = (end_lat - target_stations_df['lat'])**2 + (end_lng - target_stations_df['long'])**2
                    
                    temp_df = target_stations_df[target_stations_df['distance_d'] < total_distance]
                    # print('Route num', route_num)
                    # print('Line', line)
                    # print("****",stop_num)
                    # print(temp_df)
                    # if first stop, remove the cloest
                    if stop_num == 1:
                        temp_df = temp_df.sort_values(by = 'distance', ascending=True)[1:]

                    # keep the one closer to destination ]
                    # print(temp_df.shape)
                    if temp_df.shape[0] == 0:
                        continue
                    min_index = temp_df['distance'].idxmin()
                    print(temp_df['station'][min_index])
                    # give values to the dict
                    stop_info_dict['station'] = temp_df['station'][min_index]
                    stop_info_dict['station_type'] = 'mid'
                    stop_info_dict['lat'] = temp_df['lat'][min_index]
                    stop_info_dict['long'] = temp_df['long'][min_index]

                    # update current lat and long
                    current_lat = temp_df['lat'][min_index]
                    current_lng = temp_df['long'][min_index]
                
                # update stop_info_dict
                stops_dict[stop_num] = stop_info_dict
                # stop_info_dict = {}
                total_distance = (current_lat - end_lat)**2 + (current_lng - end_lng)**2
            
            # generate df from dict
            stops_df_list = []
            for stops_num in stops_dict:
                temp_df = pd.DataFrame.from_dict([stops_dict[stops_num]]).reset_index(drop = True)
                temp_df['stops_num'] = stops_num
                stops_df_list.append(temp_df)
            stops_df = pd.concat(stops_df_list, ignore_index=True)
            stops_df['route_num'] = route_num
            stops_dfs.append(stops_df)

        stops_dfs = pd.concat(stops_dfs, ignore_index=True)

        return stops_dfs

# %%
        
mygoogle_routes = google_routes()
directions_df = mygoogle_routes.get_directions()
stops_df = mygoogle_routes.get_stops()
stops_df

# # %%
# def get_stops(directions_dfs, stations_df = stations_df):
#     stops_dfs = []
#     for i in range(directions_dfs.shape[0]):
#         directions_df = directions_dfs.iloc[[i],].reset_index(drop = True)
#         print(directions_df)
#         route_num = directions_df['route_num'][0]
#         line = directions_df['line'][0]
#         num_stops = directions_df['num_stops'][0]
#         target_stations_df = stations_df[stations_df['linename'].str.contains(line[0])]
        
#         stops_dict = {}
#         stop_info_dict = {'line':line}
#         current_lat = directions_df['departure_stop_lat'][0]
#         current_lng = directions_df['departure_stop_lng'][0]

#         end_lat = directions_df['arrival_stop_lat'][0]
#         end_lng = directions_df['arrival_stop_lng'][0]

#         total_distance = (current_lat - end_lat)**2 + (current_lng - end_lng)**2

#         for stop_num in range(num_stops+1):
#             # print(stop_num)
#             # the start location
#             if stop_num == 0:
#                 stop_info_dict['station'] = directions_df['departure_stop'][0]
#                 stop_info_dict['station_type'] = 'start'
#                 stop_info_dict['lat'] = directions_df['departure_stop_lat'][0]
#                 stop_info_dict['long'] = directions_df['departure_stop_lng'][0]
            
#             # the last station
#             elif stop_num == num_stops:
#                 stop_info_dict['station'] = directions_df['arrival_stop'][0]
#                 stop_info_dict['station_type'] = 'end'
#                 stop_info_dict['lat'] = end_lat
#                 stop_info_dict['long'] = end_lng
        
#             # other stations in the middle, needs some calculation
#             else:
                
#                 # candidates must make the distance shorter
#                 target_stations_df['distance'] = (current_lat - target_stations_df['lat'])**2 + (current_lng - target_stations_df['long'])**2 
#                 target_stations_df['distance_d'] = (end_lat - target_stations_df['lat'])**2 + (end_lng - target_stations_df['long'])**2
                
#                 temp_df = target_stations_df[target_stations_df['distance_d'] < total_distance]
                
#                 # keep the one closer to destination ]
#                 print(temp_df.shape)
#                 min_index = temp_df['distance'].idxmin()

#                 # give values to the dict
#                 stop_info_dict['station'] = temp_df['station'][min_index]
#                 stop_info_dict['station_type'] = 'mid'
#                 stop_info_dict['lat'] = temp_df['lat'][min_index]
#                 stop_info_dict['long'] = temp_df['long'][min_index]

#                 # update current lat and long
#                 current_lat = temp_df['lat'][min_index]
#                 current_lng = temp_df['long'][min_index]
            
#             # update stop_info_dict
#             stops_dict[stop_num] = stop_info_dict
#             stop_info_dict = {'line':line}
#             total_distance = (current_lat - end_lat)**2 + (current_lng - end_lng)**2
        
#         # generate df from dict
#         stops_df_list = []
#         for stops_num in stops_dict:
#             temp_df = pd.DataFrame.from_dict([stops_dict[stops_num]]).reset_index(drop = True)
#             temp_df['stops_num'] = stops_num
#             stops_df_list.append(temp_df)
#         stops_df = pd.concat(stops_df_list, ignore_index=True)
#         stops_df['route_num'] = route_num
#         stops_dfs.append(stops_df)

#     stops_dfs = pd.concat(stops_dfs, ignore_index=True)
#     return stops_dfs




# # %% try this class
# mygoogle_routes = google_routes()
# directions_df = mygoogle_routes.get_directions()
# directions_df

# # %%
# key = 'AIzaSyAGrKCa5wYrYmkhiFQcKQ27oz0_jOivtkE'
# start_location = '168 St, New York, NY 10032'
# destination = 'JFK, NY 11101'
# departure_time = datetime.datetime.now()

# gmaps = googlemaps.Client(key = key)
# directions = gmaps.directions(start_location, destination, mode = 'transit', 
#                               transit_mode = 'subway', alternatives = True,  departure_time = departure_time)



# %%
