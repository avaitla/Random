import PIL.Image
import PIL.ImageOps
import math
import copy

class SEAMCARVING(object):
    def __init__(self, dataform, record=False):
        if(isinstance(dataform, str)):
            dataform=PIL.Image.open(dataform)
            
        data=dataform.getdata()
        self.colorpixels=dict()
        self.cols, self.rows=dataform.size
        self.originalcols=self.cols
        self.originalrows=self.rows
        self.record=record

        if(self.record):
            self.recordkeeping=dict()
            for y in range(self.rows):
                for x in range(self.cols): 
                    self.recordkeeping[(x,y)]=data[x+y*self.cols]
    
        for y in range(self.rows):
            for x in range(self.cols): 
                self.colorpixels[(x,y)]=data[x+y*self.cols]
        print("Loaded Pixels")
        gray=PIL.ImageOps.grayscale(dataform)
        data=gray.getdata()
        print("Developed Grayscale Map")

        self.pixels=dict()
        for y in range(self.rows):
            for x in range(self.cols): 
                self.pixels[(x,y)]=data[x+y*self.cols]
        print("Developed Pixel Dictionary")

        self.makefullgradient()
        print("Developed Energy Function")
        

    def sobel(self, pixel):
        gx = ((-1, 0, 1), (-2, 0, 2), (-1, 0, 1))
        gy = ((-1, -2, -1), (0, 0, 0), (1, 2, 1))
        if(pixel[0]==0 or pixel[0]==self.cols-1 or pixel[1]==0 or pixel[1]==self.rows-1):
            return 1500
        else:
            pixelx=pixel[0]
            pixely=pixel[1]
            sumx=sumy=0
            for y in range(-1,2):
                for x in range(-1,2):
                    sumx+=self.pixels[(pixelx+y, pixely+x)]*gx[y+1][x+1]
                    sumy+=self.pixels[(pixelx+y, pixely+x)]*gy[y+1][x+1]
            value=int(math.sqrt(sumx*sumx+sumy*sumy))
            return value

    def exportimage(self, filename, source, cols, rows, form="RGB"):
        out_image=PIL.Image.new(form, (cols, rows), None)
        outdata=out_image.load()
        for y in range(rows):
            for x in range(cols):
                outdata[x, y]=source[(x,y)]
        if(filename[-3:]!=".png"):
            filename=filename+".png"
        out_image.save(filename, "PNG")

    def drawseams(self, seamtype, n=1, seamcolor=(255,255,255), record=False):
        self.computeseamgradient(seamtype)
        if(seamtype=="vertical"):
            if(n>self.cols): n=self.cols
        elif(seamtype=="horizontal"):
            if(n>self.rows): n=self.rows

        if(record):
            pixels=self.getsingleseam(seamtype)
            if(seamtype=="vertical"):
                change=self.originalcols-self.cols
                for pix in pixels:
                    self.recordkeeping[(pix[0]+change, pix[1])]=seamcolor
            elif(seamtype=="horizontal"):
                change=self.originalrows-self.rows
                for pix in pixels:
                    self.recordkeeping[(pix[0], pix[1]+change)]=seamcolor

        else:
            points=self.sortedseamcosts(seamtype)
            for start in points[:n]:
                pixels=self.backtracker(seamtype, start)
                for pix in pixels:
                    self.colorpixels[pix]=seamcolor

    def makefullgradient(self):
        self.gradients=dict()
        for y in range(self.rows):
            for x in range(self.cols):
                self.gradients[(x,y)]=self.sobel((x,y))

    def findmin(self, pixel, seamtype):
        values=list()
        minpath=0
        minvalue=0
        x=pixel[0]
        y=pixel[1]
        if(seamtype=="vertical"):
            minvalue=self.verticalseamcost[(x,y-1)]
            try:
                if(self.verticalseamcost[(x+1, y-1)]<minvalue):
                    minpath=1
                    minvalue=self.verticalseamcost[(x+1, y-1)]
            except KeyError: pass
            try:
                if(self.verticalseamcost[(x-1, y-1)]<minvalue):
                    minpath=-1
                    minvalue=self.verticalseamcost[(x-1, y-1)]
            except KeyError: pass
            
        elif(seamtype=="horizontal"):
            minvalue=self.horizontalseamcost[(x-1, y)]
            try:
                if(self.horizontalseamcost[(x-1, y+1)]<minvalue):
                    minpath=1
                    minvalue=self.horizontalseamcost[(x-1, y+1)]
            except KeyError: pass
            try:
                if(self.horizontalseamcost[(x-1, y-1)]<minvalue):
                    minpath=-1
                    minvalue=self.horizontalseamcost[(x-1, y-1)]
            except KeyError: pass
        return minvalue, minpath

    def computeseamgradient(self, seamtype):
        if(seamtype=="vertical"):
            self.verticalseamcost=dict()
            self.verticalseambacktrack=dict()
            for x in range(self.cols):
                self.verticalseamcost[(x,0)]=self.gradients[(x,0)]
            for y in range(1, self.rows):
                for x in range(self.cols):
                    mincost, path=self.findmin((x,y), "vertical")
                    self.verticalseamcost[(x,y)]=mincost+self.gradients[(x, y)]
                    self.verticalseambacktrack[(x,y)]=path
        elif(seamtype=="horizontal"):
            self.horizontalseamcost=dict()
            self.horizontalseambacktrack=dict()
            for y in range(self.rows):
                self.horizontalseamcost[(0,y)]=self.gradients[(0,y)]
            for x in range(1, self.cols):
                for y in range(self.rows):
                    mincost, path=self.findmin((x,y), "horizontal")
                    self.horizontalseamcost[(x,y)]=mincost+self.gradients[(x, y)]
                    self.horizontalseambacktrack[(x,y)]=path

    def sortedseamcosts(self, seamtype):
        if(seamtype=="horizontal"):
            temp=[(y, self.horizontalseamcost[(self.cols-1, y)]) for y in range(self.rows)]
            temp.sort(lambda x,y: int(x[1]-y[1]))
            return map(lambda s: s[0], temp)
        if(seamtype=="vertical"):
            temp=[(x, self.verticalseamcost[(x, self.rows-1)]) for x in range(self.cols)]
            temp.sort(lambda x,y: int(x[1]-y[1]))
            return map(lambda s: s[0], temp)

    def backtracker(self, seamtype, minpath):
        vertexseams=list()
        if(seamtype=="horizontal"):
            for x in reversed(range(self.cols)):
                vertexseams.append((x, minpath))
                try: minpath+=self.horizontalseambacktrack[(x, minpath)]
                except KeyError: pass
            return vertexseams
        if(seamtype=="vertical"):
            for y in reversed(range(self.rows)):
                vertexseams.append((minpath, y))
                try: minpath+=self.verticalseambacktrack[(minpath, y)]
                except KeyError: pass
            return vertexseams

    def getsingleseam(self, seamtype):
        minpath=0
        if(seamtype=="horizontal"):
            mincost=self.horizontalseamcost[(self.cols-1, 0)]
            for y in range(1, self.rows):
                if(self.horizontalseamcost[(self.cols-1, y)]<mincost):
                    minpath=y
                    mincost=self.horizontalseamcost[(self.cols-1, y)]
            return self.backtracker(seamtype, minpath)

        if(seamtype=="vertical"):
            mincost=self.verticalseamcost[(0, self.rows-1)]
            for x in range(1, self.cols):
                if(self.verticalseamcost[(x, self.rows-1)]<mincost):
                    minpath=x
                    mincost=self.verticalseamcost[(x,self.rows-1)]
            return self.backtracker(seamtype, minpath)

    def calculategradients(self, pixelset):
        for pixel in pixelset:
            self.gradients[pixel]=self.sobel(pixel)

    def removeseam(self, seamtype, returnval=False):
        if(seamtype=="horizontal"):
            removalpixels=self.getsingleseam("horizontal")
            correctionpixels=list()
            for pixel in removalpixels:
                y=pixel[1]
                x=pixel[0]
                try: correctionpixels.append((x, y+1))
                except: pass
                try: correctionpixels.append((x, y-1))
                except: pass
                for i in range(y, self.rows-1):
                    self.pixels[(x, i)]=self.pixels[(x, i+1)]
                    self.colorpixels[(x, i)]=self.colorpixels[(x, i+1)]
                    self.gradients[(x, i)]=self.gradients[(x, i+1)]
                del self.colorpixels[(x, self.rows-1)]
                del self.gradients[(x, self.rows-1)]
            self.rows-=1
            self.calculategradients(correctionpixels)
            if(returnval):
                return removalpixels
                
        elif(seamtype=="vertical"):
            removalpixels=self.getsingleseam("vertical")
            correctionpixels=list()
            for pixel in removalpixels:
                y=pixel[1]
                x=pixel[0]
                try: correctionpixels.append((x+1,y))
                except: pass
                try: correctionpixels.append((x-1,y))
                except: pass
                for i in range(x, self.cols-1):
                    self.pixels[(i, y)]=self.pixels[(i+1, y)]
                    self.colorpixels[(i, y)]=self.colorpixels[(i+1, y)]
                    self.gradients[(i, y)]=self.gradients[(i+1, y)]
                del self.colorpixels[(self.cols-1, y)]
                del self.gradients[(self.cols-1, y)]
            self.cols-=1
            self.calculategradients(correctionpixels)
            if(returnval):
                return removalpixels
            
    def insertseam(self, seam, seamtype):
        if(seamtype=="horizontal"):
            for pixel in seam:
                try: top=self.colorpixels[(pixel[0], pixel[1]+1)]
                except: top=(255,255,255)
                try: bottom=self.colorpixels[(pixel[0], pixel[1]-1)]
                except: bottom=(255,255,255)
                color=((top[0]+bottom[0])/2, (top[1]+bottom[1])/2, (top[2]+bottom[2])/2)
                for pix in reversed(range(pixel[1]+1, self.rows+1)):
                    self.colorpixels[(pixel[0], pix)]=self.colorpixels[(pixel[0], pix-1)]
                self.colorpixels[pixel]=color
            self.rows+=1
            
        elif(seamtype=="vertical"):
            for pixel in seam:
                try: right=self.colorpixels[(pixel[0]+1, pixel[1])]
                except: right=(255,255,255)
                try: left=self.colorpixels[(pixel[0]-1, pixel[1])]
                except: left=(255,255,255)
                color=((right[0]+left[0])/2, (right[1]+left[1])/2, (right[2]+left[2])/2)
                for pix in reversed(range(pixel[0]+1, self.cols+1)):
                    self.colorpixels[(pix, pixel[1])]=self.colorpixels[(pix-1, pixel[1])]
                self.colorpixels[pixel]=color
            self.cols+=1
                
    def runstepinsertion(self, seamtype, n, steps=10, export=False, filename=""):
        for i in range(n%steps):
            self.runinsertion(seamtype, n%steps, export, filename)
        for i in range(n/steps):
            self.runinsertion(seamtype, steps, export, filename)
            

    def runinsertion(self, seamtype, n, export=False, filename=""):
        tempcolorpixels=copy.deepcopy(self.colorpixels)
        seamstobeinserted=list()
        for i in range(n):
            self.computeseamgradient(seamtype)
            temp=self.removeseam(seamtype, True)
            if(seamtype=="horizontal"):
                change=self.originalrows - self.rows+1
                seamstobeinserted.append([(pixel[0], pixel[1]+change) for pixel in temp])
            if(seamtype=="vertical"):
                change=self.originalcols - self.cols+1
                seamstobeinserted.append([(pixel[0]+change, pixel[1]) for pixel in temp])
            print("%d Insertable Seam(s) Found" % (i+1)) 
        
        self.colorpixels=tempcolorpixels
        self.cols=self.originalcols
        self.rows=self.originalrows

        for i, seam in enumerate(seamstobeinserted):
            self.insertseam(seam, seamtype)
            print("%d seam(s) inserted" % (i+1))

        self.originalcols=self.cols
        self.originalrows=self.rows

        newimage=PIL.Image.new("RGB", (self.cols, self.rows), None)
        newdata=newimage.load()
        for y in range(self.rows):
            for x in range(self.cols):
                newdata[x, y]=self.colorpixels[(x,y)]
        data=newimage.getdata()

        if(self.record):
            self.recordkeeping=dict()
            for y in range(self.rows):
                for x in range(self.cols): 
                    self.recordkeeping[(x,y)]=data[x+y*self.cols]
    
        gray=PIL.ImageOps.grayscale(newimage)
        data2=gray.getdata()
    
        self.pixels=dict()
        for y in range(self.rows):
            for x in range(self.cols): 
                self.pixels[(x,y)]=data2[x+y*self.cols]
                
        self.makefullgradient()
        
        if(export):
            self.exportimage(filename, self.colorpixels, self.cols, self.rows)
        
    def runremoval(self, seamtype, n, export=False, filename=""):
        if(seamtype=="vertical"):
            if(n>self.cols): n=self.colS
        elif(seamtype=="horizontal"):
            if(n>self.rows): n=self.rows
        for i in range(n):
            self.computeseamgradient(seamtype)
            self.removeseam(seamtype)
            if(self.record):
                self.drawseams(seamtype, record=True)
            print("%d seam(s) removed" % (i+1))
        if(export):
            self.exportimage(filename, self.colorpixels, self.cols, self.rows)


if __name__=="__main__":
    myimage=SEAMCARVING("rawimage.jpg")
    
    #USAGE FOR DELETING SEAMS
    #The first argument is what type of seams you want removed
    #     the options are "horizontal" and "vertical"
    #The second argument is the number of seams of that type you want removed 
    #     (in this case we remove 200 horizontal seams)
    #The third argument is option an defaults to False
    #     it is used if you want to output the file once the seam removal
    #     is completed. If you enter True it requires that you provide
    #     the fourth filename argument as well
    #The fourth argument is the name of the file to output to if you are outputting
    myimage.runremoval("vertical", 200, True, "outputverticalremoval")


    #USAGE FOR Inserting SEAMS
    #The first argument is what type of seams you want inserted
    #     the options are "horizontal" and "vertical"
    #The second argument is the number of seams of that type you want removed 
    #     (in this case we remove 200 horizontal seams)
    #The third argument is option an defaults to False
    #     it is used if you want to output the file once the seam removal
    #     is completed. If you enter True it requires that you provide
    #     the fourth filename argument as well
    #The fourth argument is the name of the file to output to if you are outputting
    #myimage.runinsertion("vertical", 50, True, "outputverticalinsertion")

