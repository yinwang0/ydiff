##################################################################
# Demo for an AST-based diffing tool
# author: Yin Wang (yinwang0@gmail.com)
##################################################################

##################################################################
# Features:
# - Detect insertion, deletion and modification of code
# - Detect refactoring (renamed or moved code)
# - Assess similarity of code
# - Ignore comments and whitespaces
#
###################################################################
# Usage:
#
# - Mouseover any framed elements to show information
#
# - Click on Blue or White elements to match the other side.
#   Once matched, the two sides will be locked into that
#   position until next match.
#
####################################################################
# Legend of colors:
#
# - Red   : deleted
# - Green : inserted
# - Blue  : modified (mouse over to show percentage of change)
# - White : unchanged or moved
#
###################################################################




class Nil:
    def __repr__(this):
        return "()"

nil = Nil()            # singleton instance of Nil



class Cons:
    def __init__(this, first, rest):
            this.first = first
            this.rest = rest
    def __repr__(this):
        if (this.rest == nil):
            return "(" + repr(this.first) + ")"
        elif (IS(this.rest, Cons)):
            s = repr(this.rest)
            return "(" + repr(this.first) + " " + s[1:-1] + ")"
        else:
            return "(" + repr(this.first) + " . " + repr(this.rest) + ")"




def foldl(f, x, ls):
    if ls == nil:
        return x
    else:
        return foldl(f, f(x, ls.first), ls.rest)




def length(ls):
    if ls == nil:
        return 0
    else:
        return 1 + length(ls.rest)




def atomAssoc(u, v):
    return Cons(Cons(u, v), nil)




def mkList(pylist):
    if (pylist == []):
        return nil
    else:
        return Cons(pylist[0], mkList(pylist[1:]))




def toList(ls):
    ret = []
    while ls <> nil:
        ret.append(ls.first)
        ls = ls.rest
    return ret




def ext(x, v, s):
    return Cons(Cons(x, v), s)




def append(ls1, ls2):
    if (ls1 == nil):
        return ls2
    else:
        return append(ls1.rest, Cons(ls1.first, ls2))




def assq(x, s):
    while s <> nil:
        if x == s.first.first:
            return s.first
        else:
            s = s.rest
    return None

    # if (s == nil):
    #     return None
    # elif (x == s.first.first):
    #     return s.first
    # else:
    #     return assq(x, s.rest)


# lookup is unchanged, but it is moved in relative
# position to other functions.
def lookup(x, s):
    p = assq(x, s)
    if p <> None:
        return p.snd
    else:
        return None



# cmap was renamed to maplist, but the function
# has been modified significantly since renaming.
# Thus we no longer consider them to be the same
# function.
def cmap(f, ls):
    if (ls == nil):
        return nil
    else:
        return Cons(f(ls.first), cmap(f, ls.rest))


# reverse is unchanged
def reverse(ls):
    ret = nil
    while ls <> nil:
        ret = Cons(ls.first, ret)
        ls = ls.rest
    return ret



# cfilter was renamed to filterlist, but the
# function has been modified significantly since
# renaming. Thus we no longer consider them to be
# the same function.
def cfilter(f, ls):
    ret = nil
    while ls <> nil:
        if f(ls.first):
            ret = Cons(ls.first, ret)
        ls = ls.rest
    return reverse(ret)

    # if (ls == nil):
    #     return nil
    # elif f(ls.first):
    #     return Cons(ls.first, cfilter(f, ls.rest))
    # else:
    #     return cfilter(f, ls.rest)

