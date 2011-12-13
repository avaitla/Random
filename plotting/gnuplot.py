import os, sys, time
from subprocess import Popen, PIPE

def get_args():
    params = {"NumberOfStreams" : 0,
              "NumberOfWindows" : 0,
              "Window_Sample_Sizes" : [],
              "Window_Y_Ranges" : [],
              "Window_Geometries" : [],
              "Stream_Titles" : [],
              "Stream_Window_Number" : [],
              "Stream_Buffers" : []}

    usage = '''\nUsage: NumberOfStreams ( M ) NumberOfWindows ( N )
       Window_0_WindowSampleSize < ... Window_N_WindowSampleSize >
       Window_0_YRangeMin Window_0_YRangeMax < ... Window_N_YRangeMin Window_N_YRangeMax >
       Window_0_Geometry < ... Window_N_Geometry > ( WidthxHeight+XOFF+YOFF )
       Stream_0_Title < ... Stream_M_Title >
       Stream_0_Window_Number < ... Stream_M_Window_Number >\n'''

    try:
        _filename = sys.argv.pop(0)
        params["NumberOfStreams"] = int(sys.argv.pop(0))
        params["NumberOfWindows"] = int(sys.argv.pop(0))
        
        for i in range(params["NumberOfWindows"]):
            params["Window_Sample_Sizes"].append(int(sys.argv.pop(0)))

        for i in range(params["NumberOfWindows"]):
            params["Window_Y_Ranges"].append((int(sys.argv.pop(0)), int(sys.argv.pop(0))))
            
        for i in range(params["NumberOfWindows"]):
            params["Window_Geometries"].append(sys.argv.pop(0))
        
        for i in range(params["NumberOfStreams"]):
            params["Stream_Titles"].append(sys.argv.pop(0))
            params["Stream_Buffers"].append([])
        
        for i in range(params["NumberOfStreams"]):
            params["Stream_Window_Number"].append(int(sys.argv.pop(0)))
        
    except Exception:
        print usage
        sys.exit(1)
    
    return params

def main():
    params = get_args()
    numberOfStreams = params["NumberOfStreams"]
    numberOfWindows = params["NumberOfWindows"]
    sampleSizes = params["Window_Sample_Sizes"]
    ranges = params["Window_Y_Ranges"]
    geometries = params["Window_Geometries"]
    titles = params["Stream_Titles"]

    windows = [params["Stream_Window_Number"][i] for i in range(numberOfStreams)]
    
    streams = dict([(i, []) for i in range(numberOfWindows)])
    print params["Stream_Window_Number"]
    for i, item in enumerate(params["Stream_Window_Number"]): streams[item].append(i)

    gnuplots = []
    buffers = [[]] * numberOfStreams
    xcounters = [0] * numberOfStreams
    
    for window in range(params["NumberOfWindows"]):
        print geometries
        fd = Popen(("gnuplot -geometry %s" % geometries[window]), shell=True, stdin=PIPE).stdin
        gnuplots.append(fd)
                
        print >>fd, "set xtics"
        print >>fd, "set ytics"
        
        yrange = params["Window_Y_Ranges"][window]
        print >>fd, "set yrange [%d:%d]" % (yrange[0], yrange[1])
        print >>fd, "set style data linespoints"
        print >>fd, "set grid"
        print >>fd, "set term x11"

    while True:
        inp = raw_input()
        _stream, _value = inp.split(":")
        
        streamIdx = int(_stream)
        windowIdx = windows[streamIdx]
        pip = gnuplots[windowIdx]
        
        xcounter = xcounters[streamIdx]
        buffers[streamIdx] = buffers[streamIdx] + [str(xcounter) + " " + str(_value)]


        xcounters[streamIdx] += 1
        max_xcounter = xcounter
        
        for stream in streams[windowIdx]:
            if(xcounters[stream] > max_xcounter):
                max_xcounter = xcounters[stream]
        
        print >>pip, " set xrange [" + str(max_xcounter-sampleSizes[windowIdx]) + ":" + str(max_xcounter) + "]"
        plots = []
        for stream in streams[windowIdx]:
            if(len(buffers[stream]) > 0):
                plots.append("\"-\" title '%s'" % titles[stream])

        print >>pip, "plot " + (", ".join(plots))
        for stream in streams[windowIdx]:
            if(len(buffers[stream]) > 0):
                for elem in buffers[stream][::-1]:
                    print >>pip, ("%s" % elem)
                print >>pip, "e"
        
        if(len(buffers[streamIdx]) > sampleSizes[windowIdx]):
            buffers[streamIdx] = buffers[streamIdx][1:]
        
        pip.flush()

    for i in range(numberOfWindows):
        pip = gnuplots[i]
        print >>pip, "exit;"
        pip.flush()
    

# Let's assuming we are streaming from stdin
if __name__ == "__main__": main()