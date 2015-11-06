import urllib
import json

    #API to return details about the address
googleGeocodeUrl = 'http://maps.google.com/maps/api/geocode/xml?sensor=false&address='

# Function to call the API and retrieve lat and long from response  
def get_coordinates(query, from_sensor=False):
    query = query.encode('utf-8')
    #Parameters to send while calling the API
    params = {
        'address': query,
        'sensor': "true" if from_sensor else "false"
    }
    #Create the API and Call the API
    url = googleGeocodeUrl + urllib.urlencode(params)
    json_response = urllib.urlopen(url)
    response = json.loads(json_response.read())
    #Check if results are present in the response and extract the lat and long from it
    if response['results']:
        location = response['results'][0]['geometry']['location']
        latitude, longitude = location['lat'], location['lng']
        print query, latitude, longitude
    else:
        latitude, longitude = 0.0, 0.0
        print query, "<no results>"
    return latitude, longitude

#Function that contain the algorithm to calculate the displacement between two co-ordinates
from math import radians, cos, sin, asin, sqrt
def haversine(lon1, lat1, lon2, lat2):
    # convert decimal degrees to radians 
    lon1, lat1, lon2, lat2 = map(radians, [lon1, lat1, lon2, lat2])
    # haversine formula 
    dlon = lon2 - lon1 
    dlat = lat2 - lat1 
    a = sin(dlat/2)**2 + cos(lat1) * cos(lat2) * sin(dlon/2)**2
    c = 2 * asin(sqrt(a)) 
    km = 6367 * c
    return km

#Input address from user
print 'Enter the address'
add = raw_input()
lat,lon = get_coordinates(add)
