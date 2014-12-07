# -*- coding: utf-8 -*-
"""
Created on Tue Nov 11 19:18:14 2014

@author: lhc
"""

import urllib2
import json

def google_flight (origin, dest, date):   
    if (not dest):
        warning = ['No Flight avaliable']
        return warning 
        
    url = "https://www.googleapis.com/qpxExpress/v1/trips/search?key= <insert key>"
    code = {
      "request": {
    "passengers": {
      "kind": "qpxexpress#passengerCounts",
      "adultCount": 1,
    },
    "slice": [
      {
    "kind": "qpxexpress#sliceInput",
    "origin": origin,
    #"maxStops": 0,
    "destination": dest,
    "date": date,
      }
    ],
    "refundable": False,
    "solutions": 60
      }
    }
    print("in google_flight")
    
    jsonreq = json.dumps(code, encoding = 'utf-8')
    print("jsonreq=")
    print(jsonreq)
    req = urllib2.Request(url, jsonreq, {'Content-Type': 'application/json'})
    flight = urllib2.urlopen(req)
    resp = json.loads(flight.read())
    
    try:
        searchResult = resp['trips']['tripOption']
    except KeyError:
        return "There is no flights available."
    
    #airline, flight number departure time, arrival time, number of stops, prices
    flightinfo = []
    for er in searchResult:
        item = ['Flight Number', 'Departure Airport', 'Departure Time', 'Arrival Airport', 'Arrival Time', 'Number of Stops', 'Price']
        value = []
        if len(er['slice'][0]['segment']) == 1:
            number = [er['slice'][0]['segment'][0]['flight']['carrier'] + er['slice'][0]['segment'][0]['flight']['number']]
            departureair = er['slice'][0]['segment'][0]['leg'][0]['origin']
            departuretime = er['slice'][0]['segment'][0]['leg'][0]['departureTime']
            arrivalair = er['slice'][0]['segment'][0]['leg'][0]['destination']
            arrivaltime = er['slice'][0]['segment'][0]['leg'][0]['arrivalTime']
            numberofstop = len(er['slice'][0]['segment'])-1
            price = er['saleTotal']
        elif len(er['slice'][0]['segment']) == 2:
            number = [er['slice'][0]['segment'][0]['flight']['carrier'] + er['slice'][0]['segment'][0]['flight']['number']]+[er['slice'][0]['segment'][1]['flight']['carrier'] + er['slice'][0]['segment'][1]['flight']['number']]
            departureair = er['slice'][0]['segment'][0]['leg'][0]['origin']
            departuretime = er['slice'][0]['segment'][0]['leg'][0]['departureTime']
            arrivalair = er['slice'][0]['segment'][1]['leg'][0]['destination']
            arrivaltime = er['slice'][0]['segment'][1]['leg'][0]['arrivalTime']
            numberofstop = len(er['slice'][0]['segment'])-1
            price = er['saleTotal']
        else:
            number = [er['slice'][0]['segment'][0]['flight']['carrier'] + er['slice'][0]['segment'][0]['flight']['number']+ " and more then one connecting flights"]
            departureair = er['slice'][0]['segment'][0]['leg'][0]['origin']
            departuretime = er['slice'][0]['segment'][0]['leg'][0]['departureTime']
            arrivalair = er['slice'][0]['segment'][len(er['slice'][0]['segment'])-1]['leg'][0]['destination']
            arrivaltime = er['slice'][0]['segment'][len(er['slice'][0]['segment'])-1]['leg'][0]['arrivalTime']
            numberofstop = len(er['slice'][0]['segment'])-1
            price = er['saleTotal']
    
        value.append(number) 
        value.append(departureair) 
        value.append(departuretime)
        value.append(arrivalair)
        value.append(arrivaltime)
        value.append(numberofstop)
        value.append(price)
        each = zip(item, value)
        d3 = dict(each)
        flightinfo.append(d3)
    return flightinfo;



#outcome = google_flight('WAS','ABI', '2014-12-11')




