## A SortedDict is a wrapper around balancedTree with
## methods similiar to those of Julia container Dict.


type SortedDict{K,V,Ord<:Ordering} <: Associative{K,V}
    bt::BalancedTree23{K,V,Ord}

    ## Base constructor: ordering + iterable of Pairs/tuples

    function SortedDict(o::Ord, ps)
        bt1 = BalancedTree23{K,V,Ord}(o)
        s = new(bt1)
        for (k,v) in ps
            s[k] = v
        end
        return s
    end

    function SortedDict(o::Ord, ps::Pair...)
        bt1 = BalancedTree23{K,V,Ord}(o)
        s = new(bt1)
        for p in ps
            s[p.first] = p.second
        end
        return s
    end

end

# Any-Any constructors
SortedDict() = SortedDict{Any,Any,ForwardOrdering}(Forward)
SortedDict{O<:Ordering}(o::O) = SortedDict{Any,Any,O}(o)

# Constructors from Pairs
SortedDict{K,V}(ps::Pair{K,V}...)            = SortedDict{K,V,ForwardOrdering}(Forward, ps)
SortedDict{K  }(ps::Pair{K}...,)             = SortedDict{K,Any,ForwardOrdering}(Forward, ps)
SortedDict{V  }(ps::Pair{TypeVar(:K),V}...,) = SortedDict{Any,V,ForwardOrdering}(Forward, ps)
SortedDict(     ps::Pair...)                 = SortedDict{Any,Any,ForwardOrdering}(Forward, ps)

# Constructors from Ordering + Pairs
SortedDict{K,V,Ord<:Ordering}(o::Ord, ps::Pair{K,V}...)           = SortedDict{K,V,Ord}(o, ps)
SortedDict{K  ,Ord<:Ordering}(o::Ord, ps::Pair{K}...)             = SortedDict{K,Any,Ord}(o, ps)
SortedDict{V  ,Ord<:Ordering}(o::Ord, ps::Pair{TypeVar(:K),V}...) = SortedDict{Any,V,Ord}(o, ps)
SortedDict{    Ord<:Ordering}(o::Ord, ps::Pair...)                = SortedDict{Any,Any,Ord}(o, ps)

# Constuctor from Associative
SortedDict{K,V,Ord<:Ordering}(d::Associative{K,V}, o::Ord=Forward) = SortedDict{K,V,Ord}(o, d)
SortedDict{K,V,Ord<:Ordering}(o::Ord, d::Associative{K,V}) = SortedDict{K,V,Ord}(o, d)

## Constructor which takes an iterable; ordering type is optional.

function SortedDict{Ord<:Ordering}(kv, o::Ord=Forward)
    try
        sorted_dict_with_eltype(kv, eltype(kv), o)
    catch e
        if any(x->isempty(methodswith(typeof(kv), x, true)), [start, next, done]) ||
            !all(x->isa(x,Union{Tuple,Pair}),kv)
            throw(ArgumentError("SortedDict(kv): kv needs to be an iterator of tuples or pairs"))
        else
            rethrow(e)
        end
    end
end

function SortedDict{Ord<:Ordering}(o::Ord, kv)
    try
        sorted_dict_with_eltype(kv, eltype(kv), o)
    catch e
        if any(x->isempty(methodswith(typeof(kv), x, true)), [start, next, done]) ||
            !all(x->isa(x,Union{Tuple,Pair}),kv)
            throw(ArgumentError("SortedDict(kv): kv needs to be an iterator of tuples or pairs"))
        else
            rethrow(e)
        end
    end
end


function sorted_dict_with_eltype{K,V,Ord}(ps, ::Type{Pair{K,V}}, o::Ord)
    h = SortedDict{K,V,Ord}(o)
    for p in ps
        h[p.first] = p.second
    end
    h
end
sorted_dict_with_eltype{K}(ps, ::Type{Pair{K}},             o) = sorted_dict_with_eltype(ps, Pair{K,Any},   o)
sorted_dict_with_eltype{V}(ps, ::Type{Pair{TypeVar(:K),V}}, o) = sorted_dict_with_eltype(ps, Pair{Any,V},   o)
sorted_dict_with_eltype(   ps, ::Type{Pair},                o) = sorted_dict_with_eltype(ps, Pair{Any,Any}, o)

function sorted_dict_with_eltype{K,V,Ord}(kv, ::Type{Tuple{K,V}}, o::Ord)
    h = SortedDict{K,V,Ord}(o)
    for (k,v) in kv
        h[k] = v
    end
    h
end
sorted_dict_with_eltype{K}(kv, ::Type{Tuple{K}},             o) = sorted_dict_with_eltype(kv, Tuple{K,Any},   o)
sorted_dict_with_eltype{V}(kv, ::Type{Tuple{TypeVar(:K),V}}, o) = sorted_dict_with_eltype(kv, Tuple{Any,V},   o)
sorted_dict_with_eltype(   kv, ::Type,                       o) = sorted_dict_with_eltype(kv, Tuple{Any,Any}, o)

# Constructors with eltype {K,V} specified
@compat (::Type{SortedDict{K,V}}){K,V}(ps::Pair...) = SortedDict{K,V,ForwardOrdering}(Forward, ps)
@compat (::Type{SortedDict{K,V}}){K,V,Ord<:Ordering}(o::Ord, ps::Pair...) = SortedDict{K,V,Ord}(o, ps)


typealias SDSemiToken IntSemiToken

typealias SDToken Tuple{SortedDict,IntSemiToken}

## This function implements m[k]; it returns the
## data item associated with key k.

@inline function getindex(m::SortedDict, k_)
    i, exactfound = findkey(m.bt, convert(keytype(m),k_))
    !exactfound && throw(KeyError(k_))
    return m.bt.data[i].d
end


## This function implements m[k]=d; it sets the
## data item associated with key k equal to d.

@inline function setindex!{K,V,Ord<:Ordering}(m::SortedDict{K,V,Ord}, v_, k_)
    insert!(m.bt, convert(K,k_), convert(V,v_), false)
    m
end

## push! is an alternative to insert!; it returns the container.


@inline function push!{K,V}(m::SortedDict{K,V}, pr::Pair)
    insert!(m.bt, convert(K, pr[1]), convert(V, pr[2]), false)
    m
end




## This function looks up a key in the tree;
## if not found, then it returns a marker for the
## end of the tree.


@inline function find(m::SortedDict, k_)
    ll, exactfound = findkey(m.bt, convert(keytype(m),k_))
    IntSemiToken(exactfound? ll : 2)
end

## This function inserts an item into the tree.
## Unlike m[k]=d, it also returns a bool and a token.
## The bool is true if the inserted item is new.
## It is false if there was already an item
## with that key.
## The token points to the newly inserted item.


@inline function insert!{K,V,Ord<:Ordering}(m::SortedDict{K,V,Ord}, k_, v_)
    b, i = insert!(m.bt, convert(K,k_), convert(V,v_), false)
    b, IntSemiToken(i)
end



@inline eltype{K,V,Ord<:Ordering}(m::SortedDict{K,V,Ord}) =  Pair{K,V}
@inline eltype{K,V,Ord<:Ordering}(::Type{SortedDict{K,V,Ord}}) =  Pair{K,V}
@inline function in{K,V,Ord<:Ordering}(pr::Pair, m::SortedDict{K,V,Ord})
    i, exactfound = findkey(m.bt, convert(K, pr[1]))
    return exactfound && isequal(m.bt.data[i].d, convert(V, pr[2]))
end

@inline in(::Tuple{Any,Any}, ::SortedDict) =
    throw(ArgumentError("'(k,v) in sorteddict' not supported in Julia 0.4 or 0.5.  See documentation"))


@inline keytype{K,V,Ord<:Ordering}(m::SortedDict{K,V,Ord}) = K
@inline keytype{K,V,Ord<:Ordering}(::Type{SortedDict{K,V,Ord}}) = K
@inline valtype{K,V,Ord<:Ordering}(m::SortedDict{K,V,Ord}) = V
@inline valtype{K,V,Ord<:Ordering}(::Type{SortedDict{K,V,Ord}}) = V
@inline ordtype{K,V,Ord<:Ordering}(m::SortedDict{K,V,Ord}) = Ord
@inline ordtype{K,V,Ord<:Ordering}(::Type{SortedDict{K,V,Ord}}) = Ord


## First and last return the first and last (key,data) pairs
## in the SortedDict.  It is an error to invoke them on an
## empty SortedDict.


@inline function first(m::SortedDict)
    i = beginloc(m.bt)
    i == 2 && throw(BoundsError())
    return Pair(m.bt.data[i].k, m.bt.data[i].d)
end

@inline function last(m::SortedDict)
    i = endloc(m.bt)
    i == 1 && throw(BoundsError())
    return Pair(m.bt.data[i].k, m.bt.data[i].d)
end




@inline orderobject(m::SortedDict) = m.bt.ord


@inline function haskey(m::SortedDict, k_)
    i, exactfound = findkey(m.bt,convert(keytype(m),k_))
    exactfound
end

@inline function get{K,V,Ord<:Ordering}(m::SortedDict{K,V,Ord}, k_, default_)
    i, exactfound = findkey(m.bt, convert(K,k_))
   return exactfound? m.bt.data[i].d : convert(V,default_)
end


function get!{K,V,Ord<:Ordering}(m::SortedDict{K,V,Ord}, k_, default_)
    k = convert(K,k_)
    i, exactfound = findkey(m.bt, k)
    if exactfound
        return m.bt.data[i].d
    else
        default = convert(V,default_)
        insert!(m.bt,K,Vefault, false)
        return default
    end
end


function getkey{K,V,Ord<:Ordering}(m::SortedDict{K,V,Ord}, k_, default_)
    i, exactfound = findkey(m.bt, convert(K,k_))
    exactfound? m.bt.data[i].k : convert(K,Vefault_)
end

## Function delete! deletes an item at a given
## key

@inline function delete!(m::SortedDict, k_)
    i, exactfound = findkey(m.bt,convert(keytype(m),k_))
    !exactfound && throw(KeyError(k_))
    delete!(m.bt, i)
    m
end

@inline function pop!(m::SortedDict, k_)
    i, exactfound = findkey(m.bt,convert(keytype(m),k_))
    !exactfound && throw(KeyError(k_))
    d = m.bt.data[i].d
    delete!(m.bt, i)
    d
end


## Check if two SortedDicts are equal in the sense of containing
## the same (K,V) pairs.  This sense of equality does not mean
## that semitokens valid for one are also valid for the other.

function isequal(m1::SortedDict, m2::SortedDict)
    ord = orderobject(m1)
    if !isequal(ord, orderobject(m2)) || !isequal(eltype(m1), eltype(m2))
        throw(ArgumentError("Cannot use isequal for two SortedDicts unless their element types and ordering objects are equal"))
    end
    p1 = startof(m1)
    p2 = startof(m2)
    while true
        if p1 == pastendsemitoken(m1)
            return p2 == pastendsemitoken(m2)
        end
        if p2 == pastendsemitoken(m2)
            return false
        end
        k1,d1 = deref((m1,p1))
        k2,d2 = deref((m2,p2))
        if !eq(ord,k1,k2) || !isequal(d1,d2)
            return false
        end
        p1 = advance((m1,p1))
        p2 = advance((m2,p2))
    end
end


function mergetwo!{K,V,Ord<:Ordering}(m::SortedDict{K,V,Ord},
                                      m2::Associative{K,V})
    for (k,v) in m2
        m[convert(K,k)] = convert(V,v)
    end
end

function packcopy{K,V,Ord<:Ordering}(m::SortedDict{K,V,Ord})
    w = SortedDict(Dict{K,V}(),orderobject(m))
    mergetwo!(w,m)
    w
end

function packdeepcopy{K,V,Ord<:Ordering}(m::SortedDict{K,V,Ord})
    w = SortedDict(Dict{K,V}(),orderobject(m))
    for (k,v) in m
        newk = deepcopy(k)
        newv = deepcopy(v)
        w[newk] = newv
    end
    w
end


function merge!{K,V,Ord<:Ordering}(m::SortedDict{K,V,Ord},
                                     others::Associative{K,V}...)
    for o in others
        mergetwo!(m,o)
    end
end

function merge{K,V,Ord<:Ordering}(m::SortedDict{K,V,Ord},
                                    others::Associative{K,V}...)
    result = packcopy(m)
    merge!(result, others...)
    result
end



similar{K,V,Ord<:Ordering}(m::SortedDict{K,V,Ord}) =
    SortedDict{K,V,Ord}(orderobject(m))
