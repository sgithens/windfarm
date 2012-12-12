# Utility to generate a batchgeo input file from all the SITE data files.
import os

topdir = './actual/'

files = os.listdir(topdir)

def doSite(filename):
    togo = {}
    f = open(filename)
    l1 = f.readline()
    l2 = f.readline()
    parts = l2.split()
    togo['Latitude'] = parts[2]
    togo['Longitude'] = parts[4].strip(',')
    togo['Group'] = filename[14:-4]
    return togo

def doSites():
    print "Latitude\tLongitude\tGroup"
    for filename in files:
        if filename.endswith('.CSV'):
            site = doSite(topdir+filename)
            print('%s\t%s\t%s' % (site['Latitude'],site['Longitude'],site['Group']))

doSites()
