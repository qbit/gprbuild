#!/usr/bin/env python

import getopt
import pypdl
from pypdl import *


if __name__ == "__main__":
	try:
        	opts,args=getopt.getopt(sys.argv[1:],'f:h:p:o:')
		if not len(sys.argv)==9:
			print 'Usage : unzipPdl.py -f <pdl file to unzip> -o <output.pdl> -h <gprime hostname> -p <port>'
			sys.exit(1)
        except getopt.GetoptError,err:
                sys.exit(2)
	for o,a in opts:
		if o in ('-f','--file'):
			inputFile=a
		if o in ('-h','--host'):
			host=a
		if o in ('-p','--port'):
			port=int(a)
		if o in ('-o','--output'):
			zipfile=a
	d=pdl_read(inputFile)
	d['r.entity_kind']='inflate_pdl'
    	o=gnp_exec(d, 'r',host,port, 'inflate_pdl')
    	try:
		print 'end'
		pdl_error(o)
		pypdl.pdl_write(o,zipfile)
	except:
		print 'error'



