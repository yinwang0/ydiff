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





class PairIterator:
    def __init__(self, p):
        self.p = p
    def next(self):
        if self.p == nil:
            raise StopIteration
        ret = self.p.fst
        self.p = self.p.snd
        return ret




class Nil:
    def __repr__(self):
        return "()"
    def __iter__(self):
        return PairIterator(self)

nil = Nil()            # singleton instance of Nil




class Pair:
    def __init__(self, fst, snd):
            self.fst = fst
            self.snd = snd
    def __repr__(self):
        if (self.snd == nil):
            return "(" + repr(self.fst) + ")"
        elif (isinstance(self.snd, Pair)):
            s = repr(self.snd)
            return "(" + repr(self.fst) + " " + s[1:-1] + ")"
        else:
            return "(" + repr(self.fst) + " . " + repr(self.snd) + ")"
    def __iter__(self):
        return PairIterator(self)




def foldl(f, x, ls):
    ret = x
    for y in ls:
        ret = f(ret, y)
    return ret




def length(ls):
    ret = 0
    for x in ls:
        ret = ret + 1
    return ret




def assoc(u, v):
    return Pair(Pair(u, v), nil)




def slist(pylist):
    ret = nil
    for i in xrange(len(pylist)):
        ret = Pair(pylist[len(pylist)-i-1], ret)
    return ret




def pylist(ls):
    ret = []
    for x in ls:
        ret.append(x)
    return ret



# maplist was renamed from cmap, but the function
# has been modified significantly since renaming.
# Thus we no longer consider them to be the same
# function.
def maplist(f, ls):
    ret = nil
    for x in ls:
        ret = Pair(f(x), ret)
    return reverse(ret)



# filterlist was renamed from cfilter, but the
# function has been modified significantly since
# renaming. Thus we no longer consider them to be
# the same function.
def filterlist(f, ls):
    ret = nil
    for x in ls:
        if f(x):
            ret = Pair(x, ret)
    return reverse(ret)



def reverse(ls):
    ret = nil
    while ls <> nil:
        ret = Cons(ls.first, ret)
        ls = ls.rest
    return ret

# def reverse(ls):
#     ret = nil
#     for x in ls:
#         ret = Pair(x, ret)
#     return ret




def append(*lists, **kw):
    def append1(ls1, ls2):
        ret = ls2
        for x in ls1:
            ret = Pair(x, ret)
        return ret
    return foldl(append1, nil, slist(lists))




def assq(x, s):
    for p in s:
        if x == p.fst:
            return p
    return None




def ziplist(ls1, ls2):
    ret = nil
    while ls1 <> nil and ls2 <> nil:
        ret = Pair(Pair(ls1.fst, ls2.fst), ret)
        ls1 = ls1.snd
        ls2 = ls2.snd
    return reverse(ret)




# building association lists
def ext(x, v, s):
    return Pair(Pair(x, v), s)




def lookup(x, s):
    p = assq(x, s)
    if p <> None:
        return p.snd
    else:
        return None


