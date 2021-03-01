# -*- coding: utf-8 -*-
#
# Created by Pádraig Mac Carron
#
################################
#Import Libraries
import urllib.request
import urllib.error
import urllib.parse
import time
import json
################################



###################
#Initial Parameters


#Year to start scraping from
year0 = 2021



########################
#Gets page data

#User agent
user = "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/29.0.1547.66 Safari/537.36"


def connect(url):
    '''This connects to a url, the try statements are in case
        the website sees too many requests and kicks your ip off,
        initially it will try waiting 10 seconds, if that fails
        it will wait two minutes and if that fails it will not return anything

        Parameters
        ==========

        url    :    string
                    - the url to connect to

        Returns
        =======

        con    :    bytes object
        '''

    #request from url
    req = urllib.request.Request(url, headers={'User-Agent' : user})
    try:
        #connect to url
        con = urllib.request.urlopen(req)
    except urllib.error.HTTPError:
        time.sleep(10)
        try:
            con = urllib.request.urlopen(req)
        except urllib.error.HTTPError:
            return 0
    except urllib.error.URLError:
        time.sleep(120)
        try:
            con = urllib.request.urlopen(req)
        except urllib.error.HTTPError:
            return 0
                    
    return con

#####################
#Output

#Create file
output = open('RIP_daily_'+str(year0)+'.tsv','w')

#Write headers
output.write('%date\tID\taddress\ttown\tcounty\tall_addresses\n')

###########

#Year range
years = range(year0-2000,22)
#Month range
months = range(1,13)
#Day range
days = range(1,32)

#Get today's day and month
today = time.localtime().tm_mday
cur_month = time.localtime().tm_mon


#id dictionary for storing unique notice ids
ids = {}

#A set of used ids
used = set()

#Main loop to get each day
for y in years:

    # If moved on to a new year do the following
    if y+2000 != year0:
        #Close previous year's output and start a new file
        output.close()
        year0 = 2000 + y
        output = open('RIP_daily_'+str(year0)+'.tsv','w')
        output.write('%date\tID\taddress\ttown\tcounty\tall_addresses\n')

    #Go through each month
    for m in months:

        #If the month is greater than the current month stop
        if y == 20 and m > cur_month:
            break
        
        # If month is a single digit add a zero before
        if int(m)< 10:
            m = '0' + str(m)

        #Print current month and year
        print(m,y)

        #Go through each day
        for d in days:
##            print('day:',d)

            #If the date is greater than or equal to today's date stop
            if y == 20 and int(m) == cur_month and d >= today:
                #Note this will only break this loop for the days
                # then go back to the month loop
                break
            
            if d < 10:
                d = '0' + str(d)

            #Get dates from and to (for daily these are one day apart)
            dateto = '20'+str(y)+'-'+str(m)+'-'+str(int(d))
            datefrom = '20'+str(y)+'-'+str(m)+'-' + str(int(d)-1)

            #Starting page number
            start = 0
            #Successfully complete day flag
            successful = False

            #Each page can have 40 entries, therefore if there are 40 entries
            # I need to go to the next page (defined by start), however if the
            # last page has exactly 40 and there's nothing new on the
            # next page I make successful = True to end the loop
            while successful == False:
                #Get url from datefrom and dateto and connect to it
                url = 'https://rip.ie/deathnotices.php?do=get_deathnotices_pages&iDisplayStart=0'+str(start)+'&iDisplayLength=40&DateFrom='+datefrom+'+00%3A00%3A00&DateTo='+dateto+'+23%3A59%3A59'
                con = connect(url).readline().strip()

                #The output on this website is stored as a JSON
                for notice in json.loads(con)['aaData']:
                    #The ID is the 6th entry (starting at 0)
                    ID = notice[5]
                    #The town is the second entry, the county is the third and then the 
                    # first lines of the address are the 10th entry
                    address = notice[9].strip() + '\t' + notice[1].strip() + '\t' + notice[2].strip()
                    #address = ', '.join(a.strip() for a in address.split(',') if len(a) > 2)

                    #Add the notice id to the ids dictionary, if it is already there add a second address
                    if ID not in ids:
                        ids[ID] = [address]
                    else:
                        ids[ID] += [address]

                #Check if there's 40 on the current page to go to next page
                if len((json.loads(con)['aaData'])) % 40 == 0 and len((json.loads(con)['aaData'])) > 0: 
                    start += 40
                else:
                    successful = True


            #This loop writes the output
            for i in ids:
                #If the id has been used already we skip it
                if i in used:
                    continue
                #If the id only contains 1 address get that
                if len(ids[i]) == 1:
                    address = ids[i][0]
                #If it contains multiple addresses, get string of those
                elif len(ids[i]) > 1:
                    adds = ids[i]
                    if len(adds[0].split(',')) < max([len(a.split(',')) for a in adds]):
                        address = adds[1]
                    else:
                        address = adds[0]

                #Write line
                output.write(dateto+'\t' + i + '\t' + address)
                
                #If they had more than one address add the other address to the end
                if len(ids[i]) > 1:
                    output.write('\t'+str(ids[i]))
                #Write linebreak
                output.write('\n')
                #Add id to used set
                used.add(i)  
            

#            time.sleep(1)
    #time.sleep(1)

                      
        

output.close()

