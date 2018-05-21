#coding=utf-8
'''
This script reads CamelForth source files and parses the glossary
definition into a text file summary.

Created on 12.02.2012

@author: ulli hoffmann, christopherkalus
'''
import os
import glob
import codecs
import re
import string

GLOSSARYDEFS=re.compile("^;C|^;X|^;Z|^;U|^;A|===\W*$")

def parseInputFile(filename):
    """Parses glossary definitions from file 'filename' output: list of lines"""
    infile=open(filename)
    (pfad, filename)=os.path.split(filename)
    result=['',filename]
    for zeile in infile:
        if GLOSSARYDEFS.search(zeile):
            elemente = filter(lambda e:e!='', zeile.strip().split("  "))
            elemente.append(" ")
            
            if len(elemente) < 3:
                result.append("".join(map(lambda e:e.ljust(80,"=") if "===" in e else e, elemente))+" ")
            else:
                name, stackcomment, comment = map(string.strip, elemente[0:3])
                result.append("%-16s%-30s%s " % (name, stackcomment, comment))
    infile.close()
    return result


def parseInputFiles(inputfileGlob):
    """Parse glossary definitions form files matching 'glob'"""
    result=[]
    for filename in glob.glob(os.path.join(os.getcwd(), inputfileGlob)):
        print "parsing %s ..." % (filename,),
        result.extend(parseInputFile(filename))
        print "ok"
    return result
    
def writeOutputFile(filename, lines):
    print "writing %s ..." % (filename,),
    outfile=codecs.open(filename,"w","utf-8").write("\n".join(lines)+"\n")
    print "ok"

def deleteTrash(zeile):
    if zeile == "" or ';' not in zeile or "===" in zeile or "---" in zeile:
        return False
    else:
        return zeile[2:len(zeile)]
    
def getCategory(zeile):
    if len(zeile)>2:
        return zeile[0:2]
    else:
        return zeile

def makeGlossaryTrashfree(infile):  
    for zeile in infile:
        category = getCategory(zeile)
        zeile = deleteTrash(zeile)
        if zeile:
            trashFreeGlossary[zeile]=category
        else:
            continue

def sortAndPrint(infile,outfile):  
    makeGlossaryTrashfree(infile)
    print "writing Glossary_Sorted.txt ... ",
    for element in sorted(trashFreeGlossary):
        print >> outfile,trashFreeGlossary[element], element,
    print "ok"
        
def mergeFuncAndSorted(infile1,infile2,outfile):
    print "writing Glossary.txt ... ",
    for zeile in infile1:
        print >> outfile, zeile,
    print >> outfile, "\n\n","words sorted".center(60,"-"), "\n\n"
    for zeile in infile2:
        print >> outfile, zeile,
    print "ok"
    



if __name__=="__main__":
    print "GlossaryMaker.py - generate glossory summary for CamelForth"
    writeOutputFile("Glossary_functional.txt", parseInputFiles("*.asm"))
    
    trashFreeGlossary={}

    infile=codecs.open("Glossary_functional.txt","rU","utf-8")
    outfile=codecs.open("Glossary_Sorted.txt","w","utf-8")

    sortAndPrint(infile, outfile)
    infile.close()
    outfile.close()
    
    infile1=codecs.open("Glossary_functional.txt","rU","utf-8")
    infile2=codecs.open("Glossary_Sorted.txt","rU","utf-8")
    outfile=codecs.open("Glossary.txt","w","utf-8")
    mergeFuncAndSorted(infile1,infile2,outfile)

    print "done"

    
