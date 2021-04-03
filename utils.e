
---------***********  utils.e  ***********----------



-- the symbols '<' and '>' are used to remark Euphoria objects in comments

-- egual(a, b): COMPARES TWO STRINGS asimmetrically: THE BEGINNING OF THE BIGGER MUST BE IDENTICAL TO THE SMALLER one
global function egual(sequence a,sequence b)
    integer l
    l=length(a)
    if l>length(b) then
        l=length(b)
    end if
    for i=1 to l do
        if a[i]!=b[i] then
            return 0
        end if
    end for
    return 1
end function


-- sfind(x, a): SEARCH FOR STRING x INTO STRING a. Return position of first occurrence of x, otherwise 0 if x doesn't occur in a
-- Better 2013 version
global function sfind(sequence x,sequence a)
    integer imax,jmax,position
    imax=length(a)
    jmax=length(x)
    position=0
	if imax>=jmax then
	    for i = 1 to imax - jmax +1 do
	        if x[1]=a[i] then
	            position=i
	            for j=1 to jmax do
	                if not(x[j] = a[i+j-1]) then
	    	            position=0
	    	            exit
	                end if
	            end for
	        end if
	        if position=i then
				exit
	        end if
	    end for
	end if
    return position
end function

-- sfind_at_pos(x, a, pos): return 0 if x is not present in <a>, -1 in case of error
-- (pos out of range), and the starting position of <x> in <a> in case of success
global function sfind_at_pos(sequence x,sequence a,atom pos)
    integer imax,jmax,flag
    imax=length(a)
    jmax=length(x)
	if pos > imax or pos < 1 then return -1 end if
    flag=0
	if imax>=jmax then
	    for i = pos to imax - jmax +1 do
	        if x[1]=a[i] then
	            flag = i
	            for j=1 to jmax do
	                if not(x[j]=a[i+j-1]) then
	                	flag=0
	                	exit
	                end if
	            end for
	        end if
	        if flag=i then
				exit
	        end if
	    end for
	end if
    return flag
end function

global function sfind_eol_at_pos(sequence a,atom pos)
-- return 0 if <\n> is not present in <a>, -1 in case of error (pos out of range),
-- and the position of <\n> in <a> in case of success
    integer imax, flag
    imax=length(a)
	if pos > imax or pos < 1 then return -1 end if
    flag=0
    for i = pos to imax do
        if a[i] = 10 then
            flag = i
			exit
        end if
    end for
    return flag
end function

-- the same of sfind(), but returns the first position after the <x> character
global function sfind_end(sequence x,sequence a)
    integer imax,jmax,flag
    imax=length(a)
    jmax=length(x)
    flag=0
	if imax>=jmax then
	    for i = 1 to imax do
	        if x[1]=a[i] then
	            flag=i
	            for j=1 to jmax do
	                if not(x[j]=a[i+j-1]) then
	                	flag=0
	                	exit
	                end if
	            end for
	        end if
	        if flag=i then
				flag=flag+jmax
				exit
	        end if
	    end for
	end if
    return flag
end function

-- at every invocation returns a line of <filename>
global function next_line(integer filename)
		object dummy
        dummy = gets(filename)
        if sequence(dummy) then
            return dummy
        else
            return ""
        end if
end function

-- new version of "get_argument"
-- everthing isn't comma, tab, space, ";" or newline is an argument, beginning from <pos>
-- and until the next separator and return it, together with the new location
-- if pos = 0 returns {"",0}; negative <pos> (or non integer) will generate an error
global function get_argument_now(integer pos, sequence a)
    integer imax
    sequence word
    word={}
    imax=length(a)
	if pos=0 then
		return {"",0}
	end if
    while  (a[pos]=' ' or a[pos]='\t' or a[pos]=',' or a[pos]='\n' or a[pos]=';') and pos<imax do
        pos=pos+1
    end while
    while 1 do
        if (a[pos]=' ' or a[pos]='\t' or a[pos]=',' or a[pos]='\n' or a[pos]=';') then
            exit
        end if
        word=append(word,a[pos])
        if  pos>=imax then exit end if
        pos=pos+1
    end while
    return {word,pos}							--pos point to the first carachter after the argument found
end function

global function load_file(atom xfile) -- load the file "xfile" in a string, and return it
	sequence string,line
	string=""
	while 1 do
		line = next_line(xfile)
		if equal(line,"") then
			exit
		else
			string=string&line
		end if
	end while
	return string
end function

