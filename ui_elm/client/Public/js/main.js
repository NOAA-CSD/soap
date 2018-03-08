
(function() {
'use strict';

function F2(fun)
{
  function wrapper(a) { return function(b) { return fun(a,b); }; }
  wrapper.arity = 2;
  wrapper.func = fun;
  return wrapper;
}

function F3(fun)
{
  function wrapper(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  }
  wrapper.arity = 3;
  wrapper.func = fun;
  return wrapper;
}

function F4(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  }
  wrapper.arity = 4;
  wrapper.func = fun;
  return wrapper;
}

function F5(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  }
  wrapper.arity = 5;
  wrapper.func = fun;
  return wrapper;
}

function F6(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  }
  wrapper.arity = 6;
  wrapper.func = fun;
  return wrapper;
}

function F7(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  }
  wrapper.arity = 7;
  wrapper.func = fun;
  return wrapper;
}

function F8(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  }
  wrapper.arity = 8;
  wrapper.func = fun;
  return wrapper;
}

function F9(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  }
  wrapper.arity = 9;
  wrapper.func = fun;
  return wrapper;
}

function A2(fun, a, b)
{
  return fun.arity === 2
    ? fun.func(a, b)
    : fun(a)(b);
}
function A3(fun, a, b, c)
{
  return fun.arity === 3
    ? fun.func(a, b, c)
    : fun(a)(b)(c);
}
function A4(fun, a, b, c, d)
{
  return fun.arity === 4
    ? fun.func(a, b, c, d)
    : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e)
{
  return fun.arity === 5
    ? fun.func(a, b, c, d, e)
    : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f)
{
  return fun.arity === 6
    ? fun.func(a, b, c, d, e, f)
    : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g)
{
  return fun.arity === 7
    ? fun.func(a, b, c, d, e, f, g)
    : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h)
{
  return fun.arity === 8
    ? fun.func(a, b, c, d, e, f, g, h)
    : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i)
{
  return fun.arity === 9
    ? fun.func(a, b, c, d, e, f, g, h, i)
    : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

var _elm_lang$lazy$Native_Lazy = function() {

function memoize(thunk)
{
    var value;
    var isForced = false;
    return function(tuple0) {
        if (!isForced) {
            value = thunk(tuple0);
            isForced = true;
        }
        return value;
    };
}

return {
    memoize: memoize
};

}();

//import Native.Utils //

var _elm_lang$core$Native_Basics = function() {

function div(a, b)
{
	return (a / b) | 0;
}
function rem(a, b)
{
	return a % b;
}
function mod(a, b)
{
	if (b === 0)
	{
		throw new Error('Cannot perform mod 0. Division by zero error.');
	}
	var r = a % b;
	var m = a === 0 ? 0 : (b > 0 ? (a >= 0 ? r : r + b) : -mod(-a, -b));

	return m === b ? 0 : m;
}
function logBase(base, n)
{
	return Math.log(n) / Math.log(base);
}
function negate(n)
{
	return -n;
}
function abs(n)
{
	return n < 0 ? -n : n;
}

function min(a, b)
{
	return _elm_lang$core$Native_Utils.cmp(a, b) < 0 ? a : b;
}
function max(a, b)
{
	return _elm_lang$core$Native_Utils.cmp(a, b) > 0 ? a : b;
}
function clamp(lo, hi, n)
{
	return _elm_lang$core$Native_Utils.cmp(n, lo) < 0
		? lo
		: _elm_lang$core$Native_Utils.cmp(n, hi) > 0
			? hi
			: n;
}

var ord = ['LT', 'EQ', 'GT'];

function compare(x, y)
{
	return { ctor: ord[_elm_lang$core$Native_Utils.cmp(x, y) + 1] };
}

function xor(a, b)
{
	return a !== b;
}
function not(b)
{
	return !b;
}
function isInfinite(n)
{
	return n === Infinity || n === -Infinity;
}

function truncate(n)
{
	return n | 0;
}

function degrees(d)
{
	return d * Math.PI / 180;
}
function turns(t)
{
	return 2 * Math.PI * t;
}
function fromPolar(point)
{
	var r = point._0;
	var t = point._1;
	return _elm_lang$core$Native_Utils.Tuple2(r * Math.cos(t), r * Math.sin(t));
}
function toPolar(point)
{
	var x = point._0;
	var y = point._1;
	return _elm_lang$core$Native_Utils.Tuple2(Math.sqrt(x * x + y * y), Math.atan2(y, x));
}

return {
	div: F2(div),
	rem: F2(rem),
	mod: F2(mod),

	pi: Math.PI,
	e: Math.E,
	cos: Math.cos,
	sin: Math.sin,
	tan: Math.tan,
	acos: Math.acos,
	asin: Math.asin,
	atan: Math.atan,
	atan2: F2(Math.atan2),

	degrees: degrees,
	turns: turns,
	fromPolar: fromPolar,
	toPolar: toPolar,

	sqrt: Math.sqrt,
	logBase: F2(logBase),
	negate: negate,
	abs: abs,
	min: F2(min),
	max: F2(max),
	clamp: F3(clamp),
	compare: F2(compare),

	xor: F2(xor),
	not: not,

	truncate: truncate,
	ceiling: Math.ceil,
	floor: Math.floor,
	round: Math.round,
	toFloat: function(x) { return x; },
	isNaN: isNaN,
	isInfinite: isInfinite
};

}();
//import //

var _elm_lang$core$Native_Utils = function() {

// COMPARISONS

function eq(x, y)
{
	var stack = [];
	var isEqual = eqHelp(x, y, 0, stack);
	var pair;
	while (isEqual && (pair = stack.pop()))
	{
		isEqual = eqHelp(pair.x, pair.y, 0, stack);
	}
	return isEqual;
}


function eqHelp(x, y, depth, stack)
{
	if (depth > 100)
	{
		stack.push({ x: x, y: y });
		return true;
	}

	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object')
	{
		if (typeof x === 'function')
		{
			throw new Error(
				'Trying to use `(==)` on functions. There is no way to know if functions are "the same" in the Elm sense.'
				+ ' Read more about this at http://package.elm-lang.org/packages/elm-lang/core/latest/Basics#=='
				+ ' which describes why it is this way and what the better version will look like.'
			);
		}
		return false;
	}

	if (x === null || y === null)
	{
		return false
	}

	if (x instanceof Date)
	{
		return x.getTime() === y.getTime();
	}

	if (!('ctor' in x))
	{
		for (var key in x)
		{
			if (!eqHelp(x[key], y[key], depth + 1, stack))
			{
				return false;
			}
		}
		return true;
	}

	// convert Dicts and Sets to lists
	if (x.ctor === 'RBNode_elm_builtin' || x.ctor === 'RBEmpty_elm_builtin')
	{
		x = _elm_lang$core$Dict$toList(x);
		y = _elm_lang$core$Dict$toList(y);
	}
	if (x.ctor === 'Set_elm_builtin')
	{
		x = _elm_lang$core$Set$toList(x);
		y = _elm_lang$core$Set$toList(y);
	}

	// check if lists are equal without recursion
	if (x.ctor === '::')
	{
		var a = x;
		var b = y;
		while (a.ctor === '::' && b.ctor === '::')
		{
			if (!eqHelp(a._0, b._0, depth + 1, stack))
			{
				return false;
			}
			a = a._1;
			b = b._1;
		}
		return a.ctor === b.ctor;
	}

	// check if Arrays are equal
	if (x.ctor === '_Array')
	{
		var xs = _elm_lang$core$Native_Array.toJSArray(x);
		var ys = _elm_lang$core$Native_Array.toJSArray(y);
		if (xs.length !== ys.length)
		{
			return false;
		}
		for (var i = 0; i < xs.length; i++)
		{
			if (!eqHelp(xs[i], ys[i], depth + 1, stack))
			{
				return false;
			}
		}
		return true;
	}

	if (!eqHelp(x.ctor, y.ctor, depth + 1, stack))
	{
		return false;
	}

	for (var key in x)
	{
		if (!eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

var LT = -1, EQ = 0, GT = 1;

function cmp(x, y)
{
	if (typeof x !== 'object')
	{
		return x === y ? EQ : x < y ? LT : GT;
	}

	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? EQ : a < b ? LT : GT;
	}

	if (x.ctor === '::' || x.ctor === '[]')
	{
		while (x.ctor === '::' && y.ctor === '::')
		{
			var ord = cmp(x._0, y._0);
			if (ord !== EQ)
			{
				return ord;
			}
			x = x._1;
			y = y._1;
		}
		return x.ctor === y.ctor ? EQ : x.ctor === '[]' ? LT : GT;
	}

	if (x.ctor.slice(0, 6) === '_Tuple')
	{
		var ord;
		var n = x.ctor.slice(6) - 0;
		var err = 'cannot compare tuples with more than 6 elements.';
		if (n === 0) return EQ;
		if (n >= 1) { ord = cmp(x._0, y._0); if (ord !== EQ) return ord;
		if (n >= 2) { ord = cmp(x._1, y._1); if (ord !== EQ) return ord;
		if (n >= 3) { ord = cmp(x._2, y._2); if (ord !== EQ) return ord;
		if (n >= 4) { ord = cmp(x._3, y._3); if (ord !== EQ) return ord;
		if (n >= 5) { ord = cmp(x._4, y._4); if (ord !== EQ) return ord;
		if (n >= 6) { ord = cmp(x._5, y._5); if (ord !== EQ) return ord;
		if (n >= 7) throw new Error('Comparison error: ' + err); } } } } } }
		return EQ;
	}

	throw new Error(
		'Comparison error: comparison is only defined on ints, '
		+ 'floats, times, chars, strings, lists of comparable values, '
		+ 'and tuples of comparable values.'
	);
}


// COMMON VALUES

var Tuple0 = {
	ctor: '_Tuple0'
};

function Tuple2(x, y)
{
	return {
		ctor: '_Tuple2',
		_0: x,
		_1: y
	};
}

function chr(c)
{
	return new String(c);
}


// GUID

var count = 0;
function guid(_)
{
	return count++;
}


// RECORDS

function update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


//// LIST STUFF ////

var Nil = { ctor: '[]' };

function Cons(hd, tl)
{
	return {
		ctor: '::',
		_0: hd,
		_1: tl
	};
}

function append(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (xs.ctor === '[]')
	{
		return ys;
	}
	var root = Cons(xs._0, Nil);
	var curr = root;
	xs = xs._1;
	while (xs.ctor !== '[]')
	{
		curr._1 = Cons(xs._0, Nil);
		xs = xs._1;
		curr = curr._1;
	}
	curr._1 = ys;
	return root;
}


// CRASHES

function crash(moduleName, region)
{
	return function(message) {
		throw new Error(
			'Ran into a `Debug.crash` in module `' + moduleName + '` ' + regionToString(region) + '\n'
			+ 'The message provided by the code author is:\n\n    '
			+ message
		);
	};
}

function crashCase(moduleName, region, value)
{
	return function(message) {
		throw new Error(
			'Ran into a `Debug.crash` in module `' + moduleName + '`\n\n'
			+ 'This was caused by the `case` expression ' + regionToString(region) + '.\n'
			+ 'One of the branches ended with a crash and the following value got through:\n\n    ' + toString(value) + '\n\n'
			+ 'The message provided by the code author is:\n\n    '
			+ message
		);
	};
}

function regionToString(region)
{
	if (region.start.line == region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'between lines ' + region.start.line + ' and ' + region.end.line;
}


// TO STRING

function toString(v)
{
	var type = typeof v;
	if (type === 'function')
	{
		return '<function>';
	}

	if (type === 'boolean')
	{
		return v ? 'True' : 'False';
	}

	if (type === 'number')
	{
		return v + '';
	}

	if (v instanceof String)
	{
		return '\'' + addSlashes(v, true) + '\'';
	}

	if (type === 'string')
	{
		return '"' + addSlashes(v, false) + '"';
	}

	if (v === null)
	{
		return 'null';
	}

	if (type === 'object' && 'ctor' in v)
	{
		var ctorStarter = v.ctor.substring(0, 5);

		if (ctorStarter === '_Tupl')
		{
			var output = [];
			for (var k in v)
			{
				if (k === 'ctor') continue;
				output.push(toString(v[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (ctorStarter === '_Task')
		{
			return '<task>'
		}

		if (v.ctor === '_Array')
		{
			var list = _elm_lang$core$Array$toList(v);
			return 'Array.fromList ' + toString(list);
		}

		if (v.ctor === '<decoder>')
		{
			return '<decoder>';
		}

		if (v.ctor === '_Process')
		{
			return '<process:' + v.id + '>';
		}

		if (v.ctor === '::')
		{
			var output = '[' + toString(v._0);
			v = v._1;
			while (v.ctor === '::')
			{
				output += ',' + toString(v._0);
				v = v._1;
			}
			return output + ']';
		}

		if (v.ctor === '[]')
		{
			return '[]';
		}

		if (v.ctor === 'Set_elm_builtin')
		{
			return 'Set.fromList ' + toString(_elm_lang$core$Set$toList(v));
		}

		if (v.ctor === 'RBNode_elm_builtin' || v.ctor === 'RBEmpty_elm_builtin')
		{
			return 'Dict.fromList ' + toString(_elm_lang$core$Dict$toList(v));
		}

		var output = '';
		for (var i in v)
		{
			if (i === 'ctor') continue;
			var str = toString(v[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return v.ctor + output;
	}

	if (type === 'object')
	{
		if (v instanceof Date)
		{
			return '<' + v.toString() + '>';
		}

		if (v.elm_web_socket)
		{
			return '<websocket>';
		}

		var output = [];
		for (var k in v)
		{
			output.push(k + ' = ' + toString(v[k]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return '<internal structure>';
}

function addSlashes(str, isChar)
{
	var s = str.replace(/\\/g, '\\\\')
			  .replace(/\n/g, '\\n')
			  .replace(/\t/g, '\\t')
			  .replace(/\r/g, '\\r')
			  .replace(/\v/g, '\\v')
			  .replace(/\0/g, '\\0');
	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}


return {
	eq: eq,
	cmp: cmp,
	Tuple0: Tuple0,
	Tuple2: Tuple2,
	chr: chr,
	update: update,
	guid: guid,

	append: F2(append),

	crash: crash,
	crashCase: crashCase,

	toString: toString
};

}();
var _elm_lang$core$Basics$never = function (_p0) {
	never:
	while (true) {
		var _p1 = _p0;
		var _v1 = _p1._0;
		_p0 = _v1;
		continue never;
	}
};
var _elm_lang$core$Basics$uncurry = F2(
	function (f, _p2) {
		var _p3 = _p2;
		return A2(f, _p3._0, _p3._1);
	});
var _elm_lang$core$Basics$curry = F3(
	function (f, a, b) {
		return f(
			{ctor: '_Tuple2', _0: a, _1: b});
	});
var _elm_lang$core$Basics$flip = F3(
	function (f, b, a) {
		return A2(f, a, b);
	});
var _elm_lang$core$Basics$always = F2(
	function (a, _p4) {
		return a;
	});
var _elm_lang$core$Basics$identity = function (x) {
	return x;
};
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<|'] = F2(
	function (f, x) {
		return f(x);
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['|>'] = F2(
	function (x, f) {
		return f(x);
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['>>'] = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<<'] = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['++'] = _elm_lang$core$Native_Utils.append;
var _elm_lang$core$Basics$toString = _elm_lang$core$Native_Utils.toString;
var _elm_lang$core$Basics$isInfinite = _elm_lang$core$Native_Basics.isInfinite;
var _elm_lang$core$Basics$isNaN = _elm_lang$core$Native_Basics.isNaN;
var _elm_lang$core$Basics$toFloat = _elm_lang$core$Native_Basics.toFloat;
var _elm_lang$core$Basics$ceiling = _elm_lang$core$Native_Basics.ceiling;
var _elm_lang$core$Basics$floor = _elm_lang$core$Native_Basics.floor;
var _elm_lang$core$Basics$truncate = _elm_lang$core$Native_Basics.truncate;
var _elm_lang$core$Basics$round = _elm_lang$core$Native_Basics.round;
var _elm_lang$core$Basics$not = _elm_lang$core$Native_Basics.not;
var _elm_lang$core$Basics$xor = _elm_lang$core$Native_Basics.xor;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['||'] = _elm_lang$core$Native_Basics.or;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['&&'] = _elm_lang$core$Native_Basics.and;
var _elm_lang$core$Basics$max = _elm_lang$core$Native_Basics.max;
var _elm_lang$core$Basics$min = _elm_lang$core$Native_Basics.min;
var _elm_lang$core$Basics$compare = _elm_lang$core$Native_Basics.compare;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['>='] = _elm_lang$core$Native_Basics.ge;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<='] = _elm_lang$core$Native_Basics.le;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['>'] = _elm_lang$core$Native_Basics.gt;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<'] = _elm_lang$core$Native_Basics.lt;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['/='] = _elm_lang$core$Native_Basics.neq;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['=='] = _elm_lang$core$Native_Basics.eq;
var _elm_lang$core$Basics$e = _elm_lang$core$Native_Basics.e;
var _elm_lang$core$Basics$pi = _elm_lang$core$Native_Basics.pi;
var _elm_lang$core$Basics$clamp = _elm_lang$core$Native_Basics.clamp;
var _elm_lang$core$Basics$logBase = _elm_lang$core$Native_Basics.logBase;
var _elm_lang$core$Basics$abs = _elm_lang$core$Native_Basics.abs;
var _elm_lang$core$Basics$negate = _elm_lang$core$Native_Basics.negate;
var _elm_lang$core$Basics$sqrt = _elm_lang$core$Native_Basics.sqrt;
var _elm_lang$core$Basics$atan2 = _elm_lang$core$Native_Basics.atan2;
var _elm_lang$core$Basics$atan = _elm_lang$core$Native_Basics.atan;
var _elm_lang$core$Basics$asin = _elm_lang$core$Native_Basics.asin;
var _elm_lang$core$Basics$acos = _elm_lang$core$Native_Basics.acos;
var _elm_lang$core$Basics$tan = _elm_lang$core$Native_Basics.tan;
var _elm_lang$core$Basics$sin = _elm_lang$core$Native_Basics.sin;
var _elm_lang$core$Basics$cos = _elm_lang$core$Native_Basics.cos;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['^'] = _elm_lang$core$Native_Basics.exp;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['%'] = _elm_lang$core$Native_Basics.mod;
var _elm_lang$core$Basics$rem = _elm_lang$core$Native_Basics.rem;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['//'] = _elm_lang$core$Native_Basics.div;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['/'] = _elm_lang$core$Native_Basics.floatDiv;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['*'] = _elm_lang$core$Native_Basics.mul;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['-'] = _elm_lang$core$Native_Basics.sub;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['+'] = _elm_lang$core$Native_Basics.add;
var _elm_lang$core$Basics$toPolar = _elm_lang$core$Native_Basics.toPolar;
var _elm_lang$core$Basics$fromPolar = _elm_lang$core$Native_Basics.fromPolar;
var _elm_lang$core$Basics$turns = _elm_lang$core$Native_Basics.turns;
var _elm_lang$core$Basics$degrees = _elm_lang$core$Native_Basics.degrees;
var _elm_lang$core$Basics$radians = function (t) {
	return t;
};
var _elm_lang$core$Basics$GT = {ctor: 'GT'};
var _elm_lang$core$Basics$EQ = {ctor: 'EQ'};
var _elm_lang$core$Basics$LT = {ctor: 'LT'};
var _elm_lang$core$Basics$JustOneMore = function (a) {
	return {ctor: 'JustOneMore', _0: a};
};

//import Native.Utils //

var _elm_lang$core$Native_Debug = function() {

function log(tag, value)
{
	var msg = tag + ': ' + _elm_lang$core$Native_Utils.toString(value);
	var process = process || {};
	if (process.stdout)
	{
		process.stdout.write(msg);
	}
	else
	{
		console.log(msg);
	}
	return value;
}

function crash(message)
{
	throw new Error(message);
}

return {
	crash: crash,
	log: F2(log)
};

}();
var _elm_lang$core$Debug$crash = _elm_lang$core$Native_Debug.crash;
var _elm_lang$core$Debug$log = _elm_lang$core$Native_Debug.log;

var _elm_lang$core$Maybe$withDefault = F2(
	function ($default, maybe) {
		var _p0 = maybe;
		if (_p0.ctor === 'Just') {
			return _p0._0;
		} else {
			return $default;
		}
	});
var _elm_lang$core$Maybe$Nothing = {ctor: 'Nothing'};
var _elm_lang$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		var _p1 = maybeValue;
		if (_p1.ctor === 'Just') {
			return callback(_p1._0);
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$Just = function (a) {
	return {ctor: 'Just', _0: a};
};
var _elm_lang$core$Maybe$map = F2(
	function (f, maybe) {
		var _p2 = maybe;
		if (_p2.ctor === 'Just') {
			return _elm_lang$core$Maybe$Just(
				f(_p2._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map2 = F3(
	function (func, ma, mb) {
		var _p3 = {ctor: '_Tuple2', _0: ma, _1: mb};
		if (((_p3.ctor === '_Tuple2') && (_p3._0.ctor === 'Just')) && (_p3._1.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A2(func, _p3._0._0, _p3._1._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map3 = F4(
	function (func, ma, mb, mc) {
		var _p4 = {ctor: '_Tuple3', _0: ma, _1: mb, _2: mc};
		if ((((_p4.ctor === '_Tuple3') && (_p4._0.ctor === 'Just')) && (_p4._1.ctor === 'Just')) && (_p4._2.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A3(func, _p4._0._0, _p4._1._0, _p4._2._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map4 = F5(
	function (func, ma, mb, mc, md) {
		var _p5 = {ctor: '_Tuple4', _0: ma, _1: mb, _2: mc, _3: md};
		if (((((_p5.ctor === '_Tuple4') && (_p5._0.ctor === 'Just')) && (_p5._1.ctor === 'Just')) && (_p5._2.ctor === 'Just')) && (_p5._3.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A4(func, _p5._0._0, _p5._1._0, _p5._2._0, _p5._3._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map5 = F6(
	function (func, ma, mb, mc, md, me) {
		var _p6 = {ctor: '_Tuple5', _0: ma, _1: mb, _2: mc, _3: md, _4: me};
		if ((((((_p6.ctor === '_Tuple5') && (_p6._0.ctor === 'Just')) && (_p6._1.ctor === 'Just')) && (_p6._2.ctor === 'Just')) && (_p6._3.ctor === 'Just')) && (_p6._4.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A5(func, _p6._0._0, _p6._1._0, _p6._2._0, _p6._3._0, _p6._4._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});

//import Native.Utils //

var _elm_lang$core$Native_List = function() {

var Nil = { ctor: '[]' };

function Cons(hd, tl)
{
	return { ctor: '::', _0: hd, _1: tl };
}

function fromArray(arr)
{
	var out = Nil;
	for (var i = arr.length; i--; )
	{
		out = Cons(arr[i], out);
	}
	return out;
}

function toArray(xs)
{
	var out = [];
	while (xs.ctor !== '[]')
	{
		out.push(xs._0);
		xs = xs._1;
	}
	return out;
}

function foldr(f, b, xs)
{
	var arr = toArray(xs);
	var acc = b;
	for (var i = arr.length; i--; )
	{
		acc = A2(f, arr[i], acc);
	}
	return acc;
}

function map2(f, xs, ys)
{
	var arr = [];
	while (xs.ctor !== '[]' && ys.ctor !== '[]')
	{
		arr.push(A2(f, xs._0, ys._0));
		xs = xs._1;
		ys = ys._1;
	}
	return fromArray(arr);
}

function map3(f, xs, ys, zs)
{
	var arr = [];
	while (xs.ctor !== '[]' && ys.ctor !== '[]' && zs.ctor !== '[]')
	{
		arr.push(A3(f, xs._0, ys._0, zs._0));
		xs = xs._1;
		ys = ys._1;
		zs = zs._1;
	}
	return fromArray(arr);
}

function map4(f, ws, xs, ys, zs)
{
	var arr = [];
	while (   ws.ctor !== '[]'
		   && xs.ctor !== '[]'
		   && ys.ctor !== '[]'
		   && zs.ctor !== '[]')
	{
		arr.push(A4(f, ws._0, xs._0, ys._0, zs._0));
		ws = ws._1;
		xs = xs._1;
		ys = ys._1;
		zs = zs._1;
	}
	return fromArray(arr);
}

function map5(f, vs, ws, xs, ys, zs)
{
	var arr = [];
	while (   vs.ctor !== '[]'
		   && ws.ctor !== '[]'
		   && xs.ctor !== '[]'
		   && ys.ctor !== '[]'
		   && zs.ctor !== '[]')
	{
		arr.push(A5(f, vs._0, ws._0, xs._0, ys._0, zs._0));
		vs = vs._1;
		ws = ws._1;
		xs = xs._1;
		ys = ys._1;
		zs = zs._1;
	}
	return fromArray(arr);
}

function sortBy(f, xs)
{
	return fromArray(toArray(xs).sort(function(a, b) {
		return _elm_lang$core$Native_Utils.cmp(f(a), f(b));
	}));
}

function sortWith(f, xs)
{
	return fromArray(toArray(xs).sort(function(a, b) {
		var ord = f(a)(b).ctor;
		return ord === 'EQ' ? 0 : ord === 'LT' ? -1 : 1;
	}));
}

return {
	Nil: Nil,
	Cons: Cons,
	cons: F2(Cons),
	toArray: toArray,
	fromArray: fromArray,

	foldr: F3(foldr),

	map2: F3(map2),
	map3: F4(map3),
	map4: F5(map4),
	map5: F6(map5),
	sortBy: F2(sortBy),
	sortWith: F2(sortWith)
};

}();
var _elm_lang$core$List$sortWith = _elm_lang$core$Native_List.sortWith;
var _elm_lang$core$List$sortBy = _elm_lang$core$Native_List.sortBy;
var _elm_lang$core$List$sort = function (xs) {
	return A2(_elm_lang$core$List$sortBy, _elm_lang$core$Basics$identity, xs);
};
var _elm_lang$core$List$singleton = function (value) {
	return {
		ctor: '::',
		_0: value,
		_1: {ctor: '[]'}
	};
};
var _elm_lang$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
				return list;
			} else {
				var _p0 = list;
				if (_p0.ctor === '[]') {
					return list;
				} else {
					var _v1 = n - 1,
						_v2 = _p0._1;
					n = _v1;
					list = _v2;
					continue drop;
				}
			}
		}
	});
var _elm_lang$core$List$map5 = _elm_lang$core$Native_List.map5;
var _elm_lang$core$List$map4 = _elm_lang$core$Native_List.map4;
var _elm_lang$core$List$map3 = _elm_lang$core$Native_List.map3;
var _elm_lang$core$List$map2 = _elm_lang$core$Native_List.map2;
var _elm_lang$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			var _p1 = list;
			if (_p1.ctor === '[]') {
				return false;
			} else {
				if (isOkay(_p1._0)) {
					return true;
				} else {
					var _v4 = isOkay,
						_v5 = _p1._1;
					isOkay = _v4;
					list = _v5;
					continue any;
				}
			}
		}
	});
var _elm_lang$core$List$all = F2(
	function (isOkay, list) {
		return !A2(
			_elm_lang$core$List$any,
			function (_p2) {
				return !isOkay(_p2);
			},
			list);
	});
var _elm_lang$core$List$foldr = _elm_lang$core$Native_List.foldr;
var _elm_lang$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			var _p3 = list;
			if (_p3.ctor === '[]') {
				return acc;
			} else {
				var _v7 = func,
					_v8 = A2(func, _p3._0, acc),
					_v9 = _p3._1;
				func = _v7;
				acc = _v8;
				list = _v9;
				continue foldl;
			}
		}
	});
var _elm_lang$core$List$length = function (xs) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (_p4, i) {
				return i + 1;
			}),
		0,
		xs);
};
var _elm_lang$core$List$sum = function (numbers) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (x, y) {
				return x + y;
			}),
		0,
		numbers);
};
var _elm_lang$core$List$product = function (numbers) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (x, y) {
				return x * y;
			}),
		1,
		numbers);
};
var _elm_lang$core$List$maximum = function (list) {
	var _p5 = list;
	if (_p5.ctor === '::') {
		return _elm_lang$core$Maybe$Just(
			A3(_elm_lang$core$List$foldl, _elm_lang$core$Basics$max, _p5._0, _p5._1));
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List$minimum = function (list) {
	var _p6 = list;
	if (_p6.ctor === '::') {
		return _elm_lang$core$Maybe$Just(
			A3(_elm_lang$core$List$foldl, _elm_lang$core$Basics$min, _p6._0, _p6._1));
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List$member = F2(
	function (x, xs) {
		return A2(
			_elm_lang$core$List$any,
			function (a) {
				return _elm_lang$core$Native_Utils.eq(a, x);
			},
			xs);
	});
var _elm_lang$core$List$isEmpty = function (xs) {
	var _p7 = xs;
	if (_p7.ctor === '[]') {
		return true;
	} else {
		return false;
	}
};
var _elm_lang$core$List$tail = function (list) {
	var _p8 = list;
	if (_p8.ctor === '::') {
		return _elm_lang$core$Maybe$Just(_p8._1);
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List$head = function (list) {
	var _p9 = list;
	if (_p9.ctor === '::') {
		return _elm_lang$core$Maybe$Just(_p9._0);
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List_ops = _elm_lang$core$List_ops || {};
_elm_lang$core$List_ops['::'] = _elm_lang$core$Native_List.cons;
var _elm_lang$core$List$map = F2(
	function (f, xs) {
		return A3(
			_elm_lang$core$List$foldr,
			F2(
				function (x, acc) {
					return {
						ctor: '::',
						_0: f(x),
						_1: acc
					};
				}),
			{ctor: '[]'},
			xs);
	});
var _elm_lang$core$List$filter = F2(
	function (pred, xs) {
		var conditionalCons = F2(
			function (front, back) {
				return pred(front) ? {ctor: '::', _0: front, _1: back} : back;
			});
		return A3(
			_elm_lang$core$List$foldr,
			conditionalCons,
			{ctor: '[]'},
			xs);
	});
var _elm_lang$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _p10 = f(mx);
		if (_p10.ctor === 'Just') {
			return {ctor: '::', _0: _p10._0, _1: xs};
		} else {
			return xs;
		}
	});
var _elm_lang$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			_elm_lang$core$List$foldr,
			_elm_lang$core$List$maybeCons(f),
			{ctor: '[]'},
			xs);
	});
var _elm_lang$core$List$reverse = function (list) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (x, y) {
				return {ctor: '::', _0: x, _1: y};
			}),
		{ctor: '[]'},
		list);
};
var _elm_lang$core$List$scanl = F3(
	function (f, b, xs) {
		var scan1 = F2(
			function (x, accAcc) {
				var _p11 = accAcc;
				if (_p11.ctor === '::') {
					return {
						ctor: '::',
						_0: A2(f, x, _p11._0),
						_1: accAcc
					};
				} else {
					return {ctor: '[]'};
				}
			});
		return _elm_lang$core$List$reverse(
			A3(
				_elm_lang$core$List$foldl,
				scan1,
				{
					ctor: '::',
					_0: b,
					_1: {ctor: '[]'}
				},
				xs));
	});
var _elm_lang$core$List$append = F2(
	function (xs, ys) {
		var _p12 = ys;
		if (_p12.ctor === '[]') {
			return xs;
		} else {
			return A3(
				_elm_lang$core$List$foldr,
				F2(
					function (x, y) {
						return {ctor: '::', _0: x, _1: y};
					}),
				ys,
				xs);
		}
	});
var _elm_lang$core$List$concat = function (lists) {
	return A3(
		_elm_lang$core$List$foldr,
		_elm_lang$core$List$append,
		{ctor: '[]'},
		lists);
};
var _elm_lang$core$List$concatMap = F2(
	function (f, list) {
		return _elm_lang$core$List$concat(
			A2(_elm_lang$core$List$map, f, list));
	});
var _elm_lang$core$List$partition = F2(
	function (pred, list) {
		var step = F2(
			function (x, _p13) {
				var _p14 = _p13;
				var _p16 = _p14._0;
				var _p15 = _p14._1;
				return pred(x) ? {
					ctor: '_Tuple2',
					_0: {ctor: '::', _0: x, _1: _p16},
					_1: _p15
				} : {
					ctor: '_Tuple2',
					_0: _p16,
					_1: {ctor: '::', _0: x, _1: _p15}
				};
			});
		return A3(
			_elm_lang$core$List$foldr,
			step,
			{
				ctor: '_Tuple2',
				_0: {ctor: '[]'},
				_1: {ctor: '[]'}
			},
			list);
	});
var _elm_lang$core$List$unzip = function (pairs) {
	var step = F2(
		function (_p18, _p17) {
			var _p19 = _p18;
			var _p20 = _p17;
			return {
				ctor: '_Tuple2',
				_0: {ctor: '::', _0: _p19._0, _1: _p20._0},
				_1: {ctor: '::', _0: _p19._1, _1: _p20._1}
			};
		});
	return A3(
		_elm_lang$core$List$foldr,
		step,
		{
			ctor: '_Tuple2',
			_0: {ctor: '[]'},
			_1: {ctor: '[]'}
		},
		pairs);
};
var _elm_lang$core$List$intersperse = F2(
	function (sep, xs) {
		var _p21 = xs;
		if (_p21.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			var step = F2(
				function (x, rest) {
					return {
						ctor: '::',
						_0: sep,
						_1: {ctor: '::', _0: x, _1: rest}
					};
				});
			var spersed = A3(
				_elm_lang$core$List$foldr,
				step,
				{ctor: '[]'},
				_p21._1);
			return {ctor: '::', _0: _p21._0, _1: spersed};
		}
	});
var _elm_lang$core$List$takeReverse = F3(
	function (n, list, taken) {
		takeReverse:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
				return taken;
			} else {
				var _p22 = list;
				if (_p22.ctor === '[]') {
					return taken;
				} else {
					var _v23 = n - 1,
						_v24 = _p22._1,
						_v25 = {ctor: '::', _0: _p22._0, _1: taken};
					n = _v23;
					list = _v24;
					taken = _v25;
					continue takeReverse;
				}
			}
		}
	});
var _elm_lang$core$List$takeTailRec = F2(
	function (n, list) {
		return _elm_lang$core$List$reverse(
			A3(
				_elm_lang$core$List$takeReverse,
				n,
				list,
				{ctor: '[]'}));
	});
var _elm_lang$core$List$takeFast = F3(
	function (ctr, n, list) {
		if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
			return {ctor: '[]'};
		} else {
			var _p23 = {ctor: '_Tuple2', _0: n, _1: list};
			_v26_5:
			do {
				_v26_1:
				do {
					if (_p23.ctor === '_Tuple2') {
						if (_p23._1.ctor === '[]') {
							return list;
						} else {
							if (_p23._1._1.ctor === '::') {
								switch (_p23._0) {
									case 1:
										break _v26_1;
									case 2:
										return {
											ctor: '::',
											_0: _p23._1._0,
											_1: {
												ctor: '::',
												_0: _p23._1._1._0,
												_1: {ctor: '[]'}
											}
										};
									case 3:
										if (_p23._1._1._1.ctor === '::') {
											return {
												ctor: '::',
												_0: _p23._1._0,
												_1: {
													ctor: '::',
													_0: _p23._1._1._0,
													_1: {
														ctor: '::',
														_0: _p23._1._1._1._0,
														_1: {ctor: '[]'}
													}
												}
											};
										} else {
											break _v26_5;
										}
									default:
										if ((_p23._1._1._1.ctor === '::') && (_p23._1._1._1._1.ctor === '::')) {
											var _p28 = _p23._1._1._1._0;
											var _p27 = _p23._1._1._0;
											var _p26 = _p23._1._0;
											var _p25 = _p23._1._1._1._1._0;
											var _p24 = _p23._1._1._1._1._1;
											return (_elm_lang$core$Native_Utils.cmp(ctr, 1000) > 0) ? {
												ctor: '::',
												_0: _p26,
												_1: {
													ctor: '::',
													_0: _p27,
													_1: {
														ctor: '::',
														_0: _p28,
														_1: {
															ctor: '::',
															_0: _p25,
															_1: A2(_elm_lang$core$List$takeTailRec, n - 4, _p24)
														}
													}
												}
											} : {
												ctor: '::',
												_0: _p26,
												_1: {
													ctor: '::',
													_0: _p27,
													_1: {
														ctor: '::',
														_0: _p28,
														_1: {
															ctor: '::',
															_0: _p25,
															_1: A3(_elm_lang$core$List$takeFast, ctr + 1, n - 4, _p24)
														}
													}
												}
											};
										} else {
											break _v26_5;
										}
								}
							} else {
								if (_p23._0 === 1) {
									break _v26_1;
								} else {
									break _v26_5;
								}
							}
						}
					} else {
						break _v26_5;
					}
				} while(false);
				return {
					ctor: '::',
					_0: _p23._1._0,
					_1: {ctor: '[]'}
				};
			} while(false);
			return list;
		}
	});
var _elm_lang$core$List$take = F2(
	function (n, list) {
		return A3(_elm_lang$core$List$takeFast, 0, n, list);
	});
var _elm_lang$core$List$repeatHelp = F3(
	function (result, n, value) {
		repeatHelp:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
				return result;
			} else {
				var _v27 = {ctor: '::', _0: value, _1: result},
					_v28 = n - 1,
					_v29 = value;
				result = _v27;
				n = _v28;
				value = _v29;
				continue repeatHelp;
			}
		}
	});
var _elm_lang$core$List$repeat = F2(
	function (n, value) {
		return A3(
			_elm_lang$core$List$repeatHelp,
			{ctor: '[]'},
			n,
			value);
	});
var _elm_lang$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(lo, hi) < 1) {
				var _v30 = lo,
					_v31 = hi - 1,
					_v32 = {ctor: '::', _0: hi, _1: list};
				lo = _v30;
				hi = _v31;
				list = _v32;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var _elm_lang$core$List$range = F2(
	function (lo, hi) {
		return A3(
			_elm_lang$core$List$rangeHelp,
			lo,
			hi,
			{ctor: '[]'});
	});
var _elm_lang$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			_elm_lang$core$List$map2,
			f,
			A2(
				_elm_lang$core$List$range,
				0,
				_elm_lang$core$List$length(xs) - 1),
			xs);
	});

var _elm_lang$core$Result$toMaybe = function (result) {
	var _p0 = result;
	if (_p0.ctor === 'Ok') {
		return _elm_lang$core$Maybe$Just(_p0._0);
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$Result$withDefault = F2(
	function (def, result) {
		var _p1 = result;
		if (_p1.ctor === 'Ok') {
			return _p1._0;
		} else {
			return def;
		}
	});
var _elm_lang$core$Result$Err = function (a) {
	return {ctor: 'Err', _0: a};
};
var _elm_lang$core$Result$andThen = F2(
	function (callback, result) {
		var _p2 = result;
		if (_p2.ctor === 'Ok') {
			return callback(_p2._0);
		} else {
			return _elm_lang$core$Result$Err(_p2._0);
		}
	});
var _elm_lang$core$Result$Ok = function (a) {
	return {ctor: 'Ok', _0: a};
};
var _elm_lang$core$Result$map = F2(
	function (func, ra) {
		var _p3 = ra;
		if (_p3.ctor === 'Ok') {
			return _elm_lang$core$Result$Ok(
				func(_p3._0));
		} else {
			return _elm_lang$core$Result$Err(_p3._0);
		}
	});
var _elm_lang$core$Result$map2 = F3(
	function (func, ra, rb) {
		var _p4 = {ctor: '_Tuple2', _0: ra, _1: rb};
		if (_p4._0.ctor === 'Ok') {
			if (_p4._1.ctor === 'Ok') {
				return _elm_lang$core$Result$Ok(
					A2(func, _p4._0._0, _p4._1._0));
			} else {
				return _elm_lang$core$Result$Err(_p4._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p4._0._0);
		}
	});
var _elm_lang$core$Result$map3 = F4(
	function (func, ra, rb, rc) {
		var _p5 = {ctor: '_Tuple3', _0: ra, _1: rb, _2: rc};
		if (_p5._0.ctor === 'Ok') {
			if (_p5._1.ctor === 'Ok') {
				if (_p5._2.ctor === 'Ok') {
					return _elm_lang$core$Result$Ok(
						A3(func, _p5._0._0, _p5._1._0, _p5._2._0));
				} else {
					return _elm_lang$core$Result$Err(_p5._2._0);
				}
			} else {
				return _elm_lang$core$Result$Err(_p5._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p5._0._0);
		}
	});
var _elm_lang$core$Result$map4 = F5(
	function (func, ra, rb, rc, rd) {
		var _p6 = {ctor: '_Tuple4', _0: ra, _1: rb, _2: rc, _3: rd};
		if (_p6._0.ctor === 'Ok') {
			if (_p6._1.ctor === 'Ok') {
				if (_p6._2.ctor === 'Ok') {
					if (_p6._3.ctor === 'Ok') {
						return _elm_lang$core$Result$Ok(
							A4(func, _p6._0._0, _p6._1._0, _p6._2._0, _p6._3._0));
					} else {
						return _elm_lang$core$Result$Err(_p6._3._0);
					}
				} else {
					return _elm_lang$core$Result$Err(_p6._2._0);
				}
			} else {
				return _elm_lang$core$Result$Err(_p6._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p6._0._0);
		}
	});
var _elm_lang$core$Result$map5 = F6(
	function (func, ra, rb, rc, rd, re) {
		var _p7 = {ctor: '_Tuple5', _0: ra, _1: rb, _2: rc, _3: rd, _4: re};
		if (_p7._0.ctor === 'Ok') {
			if (_p7._1.ctor === 'Ok') {
				if (_p7._2.ctor === 'Ok') {
					if (_p7._3.ctor === 'Ok') {
						if (_p7._4.ctor === 'Ok') {
							return _elm_lang$core$Result$Ok(
								A5(func, _p7._0._0, _p7._1._0, _p7._2._0, _p7._3._0, _p7._4._0));
						} else {
							return _elm_lang$core$Result$Err(_p7._4._0);
						}
					} else {
						return _elm_lang$core$Result$Err(_p7._3._0);
					}
				} else {
					return _elm_lang$core$Result$Err(_p7._2._0);
				}
			} else {
				return _elm_lang$core$Result$Err(_p7._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p7._0._0);
		}
	});
var _elm_lang$core$Result$mapError = F2(
	function (f, result) {
		var _p8 = result;
		if (_p8.ctor === 'Ok') {
			return _elm_lang$core$Result$Ok(_p8._0);
		} else {
			return _elm_lang$core$Result$Err(
				f(_p8._0));
		}
	});
var _elm_lang$core$Result$fromMaybe = F2(
	function (err, maybe) {
		var _p9 = maybe;
		if (_p9.ctor === 'Just') {
			return _elm_lang$core$Result$Ok(_p9._0);
		} else {
			return _elm_lang$core$Result$Err(err);
		}
	});

//import Maybe, Native.List, Native.Utils, Result //

var _elm_lang$core$Native_String = function() {

function isEmpty(str)
{
	return str.length === 0;
}
function cons(chr, str)
{
	return chr + str;
}
function uncons(str)
{
	var hd = str[0];
	if (hd)
	{
		return _elm_lang$core$Maybe$Just(_elm_lang$core$Native_Utils.Tuple2(_elm_lang$core$Native_Utils.chr(hd), str.slice(1)));
	}
	return _elm_lang$core$Maybe$Nothing;
}
function append(a, b)
{
	return a + b;
}
function concat(strs)
{
	return _elm_lang$core$Native_List.toArray(strs).join('');
}
function length(str)
{
	return str.length;
}
function map(f, str)
{
	var out = str.split('');
	for (var i = out.length; i--; )
	{
		out[i] = f(_elm_lang$core$Native_Utils.chr(out[i]));
	}
	return out.join('');
}
function filter(pred, str)
{
	return str.split('').map(_elm_lang$core$Native_Utils.chr).filter(pred).join('');
}
function reverse(str)
{
	return str.split('').reverse().join('');
}
function foldl(f, b, str)
{
	var len = str.length;
	for (var i = 0; i < len; ++i)
	{
		b = A2(f, _elm_lang$core$Native_Utils.chr(str[i]), b);
	}
	return b;
}
function foldr(f, b, str)
{
	for (var i = str.length; i--; )
	{
		b = A2(f, _elm_lang$core$Native_Utils.chr(str[i]), b);
	}
	return b;
}
function split(sep, str)
{
	return _elm_lang$core$Native_List.fromArray(str.split(sep));
}
function join(sep, strs)
{
	return _elm_lang$core$Native_List.toArray(strs).join(sep);
}
function repeat(n, str)
{
	var result = '';
	while (n > 0)
	{
		if (n & 1)
		{
			result += str;
		}
		n >>= 1, str += str;
	}
	return result;
}
function slice(start, end, str)
{
	return str.slice(start, end);
}
function left(n, str)
{
	return n < 1 ? '' : str.slice(0, n);
}
function right(n, str)
{
	return n < 1 ? '' : str.slice(-n);
}
function dropLeft(n, str)
{
	return n < 1 ? str : str.slice(n);
}
function dropRight(n, str)
{
	return n < 1 ? str : str.slice(0, -n);
}
function pad(n, chr, str)
{
	var half = (n - str.length) / 2;
	return repeat(Math.ceil(half), chr) + str + repeat(half | 0, chr);
}
function padRight(n, chr, str)
{
	return str + repeat(n - str.length, chr);
}
function padLeft(n, chr, str)
{
	return repeat(n - str.length, chr) + str;
}

function trim(str)
{
	return str.trim();
}
function trimLeft(str)
{
	return str.replace(/^\s+/, '');
}
function trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function words(str)
{
	return _elm_lang$core$Native_List.fromArray(str.trim().split(/\s+/g));
}
function lines(str)
{
	return _elm_lang$core$Native_List.fromArray(str.split(/\r\n|\r|\n/g));
}

function toUpper(str)
{
	return str.toUpperCase();
}
function toLower(str)
{
	return str.toLowerCase();
}

function any(pred, str)
{
	for (var i = str.length; i--; )
	{
		if (pred(_elm_lang$core$Native_Utils.chr(str[i])))
		{
			return true;
		}
	}
	return false;
}
function all(pred, str)
{
	for (var i = str.length; i--; )
	{
		if (!pred(_elm_lang$core$Native_Utils.chr(str[i])))
		{
			return false;
		}
	}
	return true;
}

function contains(sub, str)
{
	return str.indexOf(sub) > -1;
}
function startsWith(sub, str)
{
	return str.indexOf(sub) === 0;
}
function endsWith(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
}
function indexes(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _elm_lang$core$Native_List.Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _elm_lang$core$Native_List.fromArray(is);
}


function toInt(s)
{
	var len = s.length;

	// if empty
	if (len === 0)
	{
		return intErr(s);
	}

	// if hex
	var c = s[0];
	if (c === '0' && s[1] === 'x')
	{
		for (var i = 2; i < len; ++i)
		{
			var c = s[i];
			if (('0' <= c && c <= '9') || ('A' <= c && c <= 'F') || ('a' <= c && c <= 'f'))
			{
				continue;
			}
			return intErr(s);
		}
		return _elm_lang$core$Result$Ok(parseInt(s, 16));
	}

	// is decimal
	if (c > '9' || (c < '0' && c !== '-' && c !== '+'))
	{
		return intErr(s);
	}
	for (var i = 1; i < len; ++i)
	{
		var c = s[i];
		if (c < '0' || '9' < c)
		{
			return intErr(s);
		}
	}

	return _elm_lang$core$Result$Ok(parseInt(s, 10));
}

function intErr(s)
{
	return _elm_lang$core$Result$Err("could not convert string '" + s + "' to an Int");
}


function toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return floatErr(s);
	}
	var n = +s;
	// faster isNaN check
	return n === n ? _elm_lang$core$Result$Ok(n) : floatErr(s);
}

function floatErr(s)
{
	return _elm_lang$core$Result$Err("could not convert string '" + s + "' to a Float");
}


function toList(str)
{
	return _elm_lang$core$Native_List.fromArray(str.split('').map(_elm_lang$core$Native_Utils.chr));
}
function fromList(chars)
{
	return _elm_lang$core$Native_List.toArray(chars).join('');
}

return {
	isEmpty: isEmpty,
	cons: F2(cons),
	uncons: uncons,
	append: F2(append),
	concat: concat,
	length: length,
	map: F2(map),
	filter: F2(filter),
	reverse: reverse,
	foldl: F3(foldl),
	foldr: F3(foldr),

	split: F2(split),
	join: F2(join),
	repeat: F2(repeat),

	slice: F3(slice),
	left: F2(left),
	right: F2(right),
	dropLeft: F2(dropLeft),
	dropRight: F2(dropRight),

	pad: F3(pad),
	padLeft: F3(padLeft),
	padRight: F3(padRight),

	trim: trim,
	trimLeft: trimLeft,
	trimRight: trimRight,

	words: words,
	lines: lines,

	toUpper: toUpper,
	toLower: toLower,

	any: F2(any),
	all: F2(all),

	contains: F2(contains),
	startsWith: F2(startsWith),
	endsWith: F2(endsWith),
	indexes: F2(indexes),

	toInt: toInt,
	toFloat: toFloat,
	toList: toList,
	fromList: fromList
};

}();

//import Native.Utils //

var _elm_lang$core$Native_Char = function() {

return {
	fromCode: function(c) { return _elm_lang$core$Native_Utils.chr(String.fromCharCode(c)); },
	toCode: function(c) { return c.charCodeAt(0); },
	toUpper: function(c) { return _elm_lang$core$Native_Utils.chr(c.toUpperCase()); },
	toLower: function(c) { return _elm_lang$core$Native_Utils.chr(c.toLowerCase()); },
	toLocaleUpper: function(c) { return _elm_lang$core$Native_Utils.chr(c.toLocaleUpperCase()); },
	toLocaleLower: function(c) { return _elm_lang$core$Native_Utils.chr(c.toLocaleLowerCase()); }
};

}();
var _elm_lang$core$Char$fromCode = _elm_lang$core$Native_Char.fromCode;
var _elm_lang$core$Char$toCode = _elm_lang$core$Native_Char.toCode;
var _elm_lang$core$Char$toLocaleLower = _elm_lang$core$Native_Char.toLocaleLower;
var _elm_lang$core$Char$toLocaleUpper = _elm_lang$core$Native_Char.toLocaleUpper;
var _elm_lang$core$Char$toLower = _elm_lang$core$Native_Char.toLower;
var _elm_lang$core$Char$toUpper = _elm_lang$core$Native_Char.toUpper;
var _elm_lang$core$Char$isBetween = F3(
	function (low, high, $char) {
		var code = _elm_lang$core$Char$toCode($char);
		return (_elm_lang$core$Native_Utils.cmp(
			code,
			_elm_lang$core$Char$toCode(low)) > -1) && (_elm_lang$core$Native_Utils.cmp(
			code,
			_elm_lang$core$Char$toCode(high)) < 1);
	});
var _elm_lang$core$Char$isUpper = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('A'),
	_elm_lang$core$Native_Utils.chr('Z'));
var _elm_lang$core$Char$isLower = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('a'),
	_elm_lang$core$Native_Utils.chr('z'));
var _elm_lang$core$Char$isDigit = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('0'),
	_elm_lang$core$Native_Utils.chr('9'));
var _elm_lang$core$Char$isOctDigit = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('0'),
	_elm_lang$core$Native_Utils.chr('7'));
var _elm_lang$core$Char$isHexDigit = function ($char) {
	return _elm_lang$core$Char$isDigit($char) || (A3(
		_elm_lang$core$Char$isBetween,
		_elm_lang$core$Native_Utils.chr('a'),
		_elm_lang$core$Native_Utils.chr('f'),
		$char) || A3(
		_elm_lang$core$Char$isBetween,
		_elm_lang$core$Native_Utils.chr('A'),
		_elm_lang$core$Native_Utils.chr('F'),
		$char));
};

var _elm_lang$core$String$fromList = _elm_lang$core$Native_String.fromList;
var _elm_lang$core$String$toList = _elm_lang$core$Native_String.toList;
var _elm_lang$core$String$toFloat = _elm_lang$core$Native_String.toFloat;
var _elm_lang$core$String$toInt = _elm_lang$core$Native_String.toInt;
var _elm_lang$core$String$indices = _elm_lang$core$Native_String.indexes;
var _elm_lang$core$String$indexes = _elm_lang$core$Native_String.indexes;
var _elm_lang$core$String$endsWith = _elm_lang$core$Native_String.endsWith;
var _elm_lang$core$String$startsWith = _elm_lang$core$Native_String.startsWith;
var _elm_lang$core$String$contains = _elm_lang$core$Native_String.contains;
var _elm_lang$core$String$all = _elm_lang$core$Native_String.all;
var _elm_lang$core$String$any = _elm_lang$core$Native_String.any;
var _elm_lang$core$String$toLower = _elm_lang$core$Native_String.toLower;
var _elm_lang$core$String$toUpper = _elm_lang$core$Native_String.toUpper;
var _elm_lang$core$String$lines = _elm_lang$core$Native_String.lines;
var _elm_lang$core$String$words = _elm_lang$core$Native_String.words;
var _elm_lang$core$String$trimRight = _elm_lang$core$Native_String.trimRight;
var _elm_lang$core$String$trimLeft = _elm_lang$core$Native_String.trimLeft;
var _elm_lang$core$String$trim = _elm_lang$core$Native_String.trim;
var _elm_lang$core$String$padRight = _elm_lang$core$Native_String.padRight;
var _elm_lang$core$String$padLeft = _elm_lang$core$Native_String.padLeft;
var _elm_lang$core$String$pad = _elm_lang$core$Native_String.pad;
var _elm_lang$core$String$dropRight = _elm_lang$core$Native_String.dropRight;
var _elm_lang$core$String$dropLeft = _elm_lang$core$Native_String.dropLeft;
var _elm_lang$core$String$right = _elm_lang$core$Native_String.right;
var _elm_lang$core$String$left = _elm_lang$core$Native_String.left;
var _elm_lang$core$String$slice = _elm_lang$core$Native_String.slice;
var _elm_lang$core$String$repeat = _elm_lang$core$Native_String.repeat;
var _elm_lang$core$String$join = _elm_lang$core$Native_String.join;
var _elm_lang$core$String$split = _elm_lang$core$Native_String.split;
var _elm_lang$core$String$foldr = _elm_lang$core$Native_String.foldr;
var _elm_lang$core$String$foldl = _elm_lang$core$Native_String.foldl;
var _elm_lang$core$String$reverse = _elm_lang$core$Native_String.reverse;
var _elm_lang$core$String$filter = _elm_lang$core$Native_String.filter;
var _elm_lang$core$String$map = _elm_lang$core$Native_String.map;
var _elm_lang$core$String$length = _elm_lang$core$Native_String.length;
var _elm_lang$core$String$concat = _elm_lang$core$Native_String.concat;
var _elm_lang$core$String$append = _elm_lang$core$Native_String.append;
var _elm_lang$core$String$uncons = _elm_lang$core$Native_String.uncons;
var _elm_lang$core$String$cons = _elm_lang$core$Native_String.cons;
var _elm_lang$core$String$fromChar = function ($char) {
	return A2(_elm_lang$core$String$cons, $char, '');
};
var _elm_lang$core$String$isEmpty = _elm_lang$core$Native_String.isEmpty;

var _elm_lang$core$Tuple$mapSecond = F2(
	function (func, _p0) {
		var _p1 = _p0;
		return {
			ctor: '_Tuple2',
			_0: _p1._0,
			_1: func(_p1._1)
		};
	});
var _elm_lang$core$Tuple$mapFirst = F2(
	function (func, _p2) {
		var _p3 = _p2;
		return {
			ctor: '_Tuple2',
			_0: func(_p3._0),
			_1: _p3._1
		};
	});
var _elm_lang$core$Tuple$second = function (_p4) {
	var _p5 = _p4;
	return _p5._1;
};
var _elm_lang$core$Tuple$first = function (_p6) {
	var _p7 = _p6;
	return _p7._0;
};

//import //

var _elm_lang$core$Native_Platform = function() {


// PROGRAMS

function program(impl)
{
	return function(flagDecoder)
	{
		return function(object, moduleName)
		{
			object['worker'] = function worker(flags)
			{
				if (typeof flags !== 'undefined')
				{
					throw new Error(
						'The `' + moduleName + '` module does not need flags.\n'
						+ 'Call ' + moduleName + '.worker() with no arguments and you should be all set!'
					);
				}

				return initialize(
					impl.init,
					impl.update,
					impl.subscriptions,
					renderer
				);
			};
		};
	};
}

function programWithFlags(impl)
{
	return function(flagDecoder)
	{
		return function(object, moduleName)
		{
			object['worker'] = function worker(flags)
			{
				if (typeof flagDecoder === 'undefined')
				{
					throw new Error(
						'Are you trying to sneak a Never value into Elm? Trickster!\n'
						+ 'It looks like ' + moduleName + '.main is defined with `programWithFlags` but has type `Program Never`.\n'
						+ 'Use `program` instead if you do not want flags.'
					);
				}

				var result = A2(_elm_lang$core$Native_Json.run, flagDecoder, flags);
				if (result.ctor === 'Err')
				{
					throw new Error(
						moduleName + '.worker(...) was called with an unexpected argument.\n'
						+ 'I tried to convert it to an Elm value, but ran into this problem:\n\n'
						+ result._0
					);
				}

				return initialize(
					impl.init(result._0),
					impl.update,
					impl.subscriptions,
					renderer
				);
			};
		};
	};
}

function renderer(enqueue, _)
{
	return function(_) {};
}


// HTML TO PROGRAM

function htmlToProgram(vnode)
{
	var emptyBag = batch(_elm_lang$core$Native_List.Nil);
	var noChange = _elm_lang$core$Native_Utils.Tuple2(
		_elm_lang$core$Native_Utils.Tuple0,
		emptyBag
	);

	return _elm_lang$virtual_dom$VirtualDom$program({
		init: noChange,
		view: function(model) { return main; },
		update: F2(function(msg, model) { return noChange; }),
		subscriptions: function (model) { return emptyBag; }
	});
}


// INITIALIZE A PROGRAM

function initialize(init, update, subscriptions, renderer)
{
	// ambient state
	var managers = {};
	var updateView;

	// init and update state in main process
	var initApp = _elm_lang$core$Native_Scheduler.nativeBinding(function(callback) {
		var model = init._0;
		updateView = renderer(enqueue, model);
		var cmds = init._1;
		var subs = subscriptions(model);
		dispatchEffects(managers, cmds, subs);
		callback(_elm_lang$core$Native_Scheduler.succeed(model));
	});

	function onMessage(msg, model)
	{
		return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback) {
			var results = A2(update, msg, model);
			model = results._0;
			updateView(model);
			var cmds = results._1;
			var subs = subscriptions(model);
			dispatchEffects(managers, cmds, subs);
			callback(_elm_lang$core$Native_Scheduler.succeed(model));
		});
	}

	var mainProcess = spawnLoop(initApp, onMessage);

	function enqueue(msg)
	{
		_elm_lang$core$Native_Scheduler.rawSend(mainProcess, msg);
	}

	var ports = setupEffects(managers, enqueue);

	return ports ? { ports: ports } : {};
}


// EFFECT MANAGERS

var effectManagers = {};

function setupEffects(managers, callback)
{
	var ports;

	// setup all necessary effect managers
	for (var key in effectManagers)
	{
		var manager = effectManagers[key];

		if (manager.isForeign)
		{
			ports = ports || {};
			ports[key] = manager.tag === 'cmd'
				? setupOutgoingPort(key)
				: setupIncomingPort(key, callback);
		}

		managers[key] = makeManager(manager, callback);
	}

	return ports;
}

function makeManager(info, callback)
{
	var router = {
		main: callback,
		self: undefined
	};

	var tag = info.tag;
	var onEffects = info.onEffects;
	var onSelfMsg = info.onSelfMsg;

	function onMessage(msg, state)
	{
		if (msg.ctor === 'self')
		{
			return A3(onSelfMsg, router, msg._0, state);
		}

		var fx = msg._0;
		switch (tag)
		{
			case 'cmd':
				return A3(onEffects, router, fx.cmds, state);

			case 'sub':
				return A3(onEffects, router, fx.subs, state);

			case 'fx':
				return A4(onEffects, router, fx.cmds, fx.subs, state);
		}
	}

	var process = spawnLoop(info.init, onMessage);
	router.self = process;
	return process;
}

function sendToApp(router, msg)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		router.main(msg);
		callback(_elm_lang$core$Native_Scheduler.succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}

function sendToSelf(router, msg)
{
	return A2(_elm_lang$core$Native_Scheduler.send, router.self, {
		ctor: 'self',
		_0: msg
	});
}


// HELPER for STATEFUL LOOPS

function spawnLoop(init, onMessage)
{
	var andThen = _elm_lang$core$Native_Scheduler.andThen;

	function loop(state)
	{
		var handleMsg = _elm_lang$core$Native_Scheduler.receive(function(msg) {
			return onMessage(msg, state);
		});
		return A2(andThen, loop, handleMsg);
	}

	var task = A2(andThen, loop, init);

	return _elm_lang$core$Native_Scheduler.rawSpawn(task);
}


// BAGS

function leaf(home)
{
	return function(value)
	{
		return {
			type: 'leaf',
			home: home,
			value: value
		};
	};
}

function batch(list)
{
	return {
		type: 'node',
		branches: list
	};
}

function map(tagger, bag)
{
	return {
		type: 'map',
		tagger: tagger,
		tree: bag
	}
}


// PIPE BAGS INTO EFFECT MANAGERS

function dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	gatherEffects(true, cmdBag, effectsDict, null);
	gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		var fx = home in effectsDict
			? effectsDict[home]
			: {
				cmds: _elm_lang$core$Native_List.Nil,
				subs: _elm_lang$core$Native_List.Nil
			};

		_elm_lang$core$Native_Scheduler.rawSend(managers[home], { ctor: 'fx', _0: fx });
	}
}

function gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.type)
	{
		case 'leaf':
			var home = bag.home;
			var effect = toEffect(isCmd, home, taggers, bag.value);
			effectsDict[home] = insert(isCmd, effect, effectsDict[home]);
			return;

		case 'node':
			var list = bag.branches;
			while (list.ctor !== '[]')
			{
				gatherEffects(isCmd, list._0, effectsDict, taggers);
				list = list._1;
			}
			return;

		case 'map':
			gatherEffects(isCmd, bag.tree, effectsDict, {
				tagger: bag.tagger,
				rest: taggers
			});
			return;
	}
}

function toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		var temp = taggers;
		while (temp)
		{
			x = temp.tagger(x);
			temp = temp.rest;
		}
		return x;
	}

	var map = isCmd
		? effectManagers[home].cmdMap
		: effectManagers[home].subMap;

	return A2(map, applyTaggers, value)
}

function insert(isCmd, newEffect, effects)
{
	effects = effects || {
		cmds: _elm_lang$core$Native_List.Nil,
		subs: _elm_lang$core$Native_List.Nil
	};
	if (isCmd)
	{
		effects.cmds = _elm_lang$core$Native_List.Cons(newEffect, effects.cmds);
		return effects;
	}
	effects.subs = _elm_lang$core$Native_List.Cons(newEffect, effects.subs);
	return effects;
}


// PORTS

function checkPortName(name)
{
	if (name in effectManagers)
	{
		throw new Error('There can only be one port named `' + name + '`, but your program has multiple.');
	}
}


// OUTGOING PORTS

function outgoingPort(name, converter)
{
	checkPortName(name);
	effectManagers[name] = {
		tag: 'cmd',
		cmdMap: outgoingPortMap,
		converter: converter,
		isForeign: true
	};
	return leaf(name);
}

var outgoingPortMap = F2(function cmdMap(tagger, value) {
	return value;
});

function setupOutgoingPort(name)
{
	var subs = [];
	var converter = effectManagers[name].converter;

	// CREATE MANAGER

	var init = _elm_lang$core$Native_Scheduler.succeed(null);

	function onEffects(router, cmdList, state)
	{
		while (cmdList.ctor !== '[]')
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = converter(cmdList._0);
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
			cmdList = cmdList._1;
		}
		return init;
	}

	effectManagers[name].init = init;
	effectManagers[name].onEffects = F3(onEffects);

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}


// INCOMING PORTS

function incomingPort(name, converter)
{
	checkPortName(name);
	effectManagers[name] = {
		tag: 'sub',
		subMap: incomingPortMap,
		converter: converter,
		isForeign: true
	};
	return leaf(name);
}

var incomingPortMap = F2(function subMap(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});

function setupIncomingPort(name, callback)
{
	var sentBeforeInit = [];
	var subs = _elm_lang$core$Native_List.Nil;
	var converter = effectManagers[name].converter;
	var currentOnEffects = preInitOnEffects;
	var currentSend = preInitSend;

	// CREATE MANAGER

	var init = _elm_lang$core$Native_Scheduler.succeed(null);

	function preInitOnEffects(router, subList, state)
	{
		var postInitResult = postInitOnEffects(router, subList, state);

		for(var i = 0; i < sentBeforeInit.length; i++)
		{
			postInitSend(sentBeforeInit[i]);
		}

		sentBeforeInit = null; // to release objects held in queue
		currentSend = postInitSend;
		currentOnEffects = postInitOnEffects;
		return postInitResult;
	}

	function postInitOnEffects(router, subList, state)
	{
		subs = subList;
		return init;
	}

	function onEffects(router, subList, state)
	{
		return currentOnEffects(router, subList, state);
	}

	effectManagers[name].init = init;
	effectManagers[name].onEffects = F3(onEffects);

	// PUBLIC API

	function preInitSend(value)
	{
		sentBeforeInit.push(value);
	}

	function postInitSend(value)
	{
		var temp = subs;
		while (temp.ctor !== '[]')
		{
			callback(temp._0(value));
			temp = temp._1;
		}
	}

	function send(incomingValue)
	{
		var result = A2(_elm_lang$core$Json_Decode$decodeValue, converter, incomingValue);
		if (result.ctor === 'Err')
		{
			throw new Error('Trying to send an unexpected type of value through port `' + name + '`:\n' + result._0);
		}

		currentSend(result._0);
	}

	return { send: send };
}

return {
	// routers
	sendToApp: F2(sendToApp),
	sendToSelf: F2(sendToSelf),

	// global setup
	effectManagers: effectManagers,
	outgoingPort: outgoingPort,
	incomingPort: incomingPort,

	htmlToProgram: htmlToProgram,
	program: program,
	programWithFlags: programWithFlags,
	initialize: initialize,

	// effect bags
	leaf: leaf,
	batch: batch,
	map: F2(map)
};

}();

//import Native.Utils //

var _elm_lang$core$Native_Scheduler = function() {

var MAX_STEPS = 10000;


// TASKS

function succeed(value)
{
	return {
		ctor: '_Task_succeed',
		value: value
	};
}

function fail(error)
{
	return {
		ctor: '_Task_fail',
		value: error
	};
}

function nativeBinding(callback)
{
	return {
		ctor: '_Task_nativeBinding',
		callback: callback,
		cancel: null
	};
}

function andThen(callback, task)
{
	return {
		ctor: '_Task_andThen',
		callback: callback,
		task: task
	};
}

function onError(callback, task)
{
	return {
		ctor: '_Task_onError',
		callback: callback,
		task: task
	};
}

function receive(callback)
{
	return {
		ctor: '_Task_receive',
		callback: callback
	};
}


// PROCESSES

function rawSpawn(task)
{
	var process = {
		ctor: '_Process',
		id: _elm_lang$core$Native_Utils.guid(),
		root: task,
		stack: null,
		mailbox: []
	};

	enqueue(process);

	return process;
}

function spawn(task)
{
	return nativeBinding(function(callback) {
		var process = rawSpawn(task);
		callback(succeed(process));
	});
}

function rawSend(process, msg)
{
	process.mailbox.push(msg);
	enqueue(process);
}

function send(process, msg)
{
	return nativeBinding(function(callback) {
		rawSend(process, msg);
		callback(succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}

function kill(process)
{
	return nativeBinding(function(callback) {
		var root = process.root;
		if (root.ctor === '_Task_nativeBinding' && root.cancel)
		{
			root.cancel();
		}

		process.root = null;

		callback(succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}

function sleep(time)
{
	return nativeBinding(function(callback) {
		var id = setTimeout(function() {
			callback(succeed(_elm_lang$core$Native_Utils.Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}


// STEP PROCESSES

function step(numSteps, process)
{
	while (numSteps < MAX_STEPS)
	{
		var ctor = process.root.ctor;

		if (ctor === '_Task_succeed')
		{
			while (process.stack && process.stack.ctor === '_Task_onError')
			{
				process.stack = process.stack.rest;
			}
			if (process.stack === null)
			{
				break;
			}
			process.root = process.stack.callback(process.root.value);
			process.stack = process.stack.rest;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_fail')
		{
			while (process.stack && process.stack.ctor === '_Task_andThen')
			{
				process.stack = process.stack.rest;
			}
			if (process.stack === null)
			{
				break;
			}
			process.root = process.stack.callback(process.root.value);
			process.stack = process.stack.rest;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_andThen')
		{
			process.stack = {
				ctor: '_Task_andThen',
				callback: process.root.callback,
				rest: process.stack
			};
			process.root = process.root.task;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_onError')
		{
			process.stack = {
				ctor: '_Task_onError',
				callback: process.root.callback,
				rest: process.stack
			};
			process.root = process.root.task;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_nativeBinding')
		{
			process.root.cancel = process.root.callback(function(newRoot) {
				process.root = newRoot;
				enqueue(process);
			});

			break;
		}

		if (ctor === '_Task_receive')
		{
			var mailbox = process.mailbox;
			if (mailbox.length === 0)
			{
				break;
			}

			process.root = process.root.callback(mailbox.shift());
			++numSteps;
			continue;
		}

		throw new Error(ctor);
	}

	if (numSteps < MAX_STEPS)
	{
		return numSteps + 1;
	}
	enqueue(process);

	return numSteps;
}


// WORK QUEUE

var working = false;
var workQueue = [];

function enqueue(process)
{
	workQueue.push(process);

	if (!working)
	{
		setTimeout(work, 0);
		working = true;
	}
}

function work()
{
	var numSteps = 0;
	var process;
	while (numSteps < MAX_STEPS && (process = workQueue.shift()))
	{
		if (process.root)
		{
			numSteps = step(numSteps, process);
		}
	}
	if (!process)
	{
		working = false;
		return;
	}
	setTimeout(work, 0);
}


return {
	succeed: succeed,
	fail: fail,
	nativeBinding: nativeBinding,
	andThen: F2(andThen),
	onError: F2(onError),
	receive: receive,

	spawn: spawn,
	kill: kill,
	sleep: sleep,
	send: F2(send),

	rawSpawn: rawSpawn,
	rawSend: rawSend
};

}();
var _elm_lang$core$Platform_Cmd$batch = _elm_lang$core$Native_Platform.batch;
var _elm_lang$core$Platform_Cmd$none = _elm_lang$core$Platform_Cmd$batch(
	{ctor: '[]'});
var _elm_lang$core$Platform_Cmd_ops = _elm_lang$core$Platform_Cmd_ops || {};
_elm_lang$core$Platform_Cmd_ops['!'] = F2(
	function (model, commands) {
		return {
			ctor: '_Tuple2',
			_0: model,
			_1: _elm_lang$core$Platform_Cmd$batch(commands)
		};
	});
var _elm_lang$core$Platform_Cmd$map = _elm_lang$core$Native_Platform.map;
var _elm_lang$core$Platform_Cmd$Cmd = {ctor: 'Cmd'};

var _elm_lang$core$Platform_Sub$batch = _elm_lang$core$Native_Platform.batch;
var _elm_lang$core$Platform_Sub$none = _elm_lang$core$Platform_Sub$batch(
	{ctor: '[]'});
var _elm_lang$core$Platform_Sub$map = _elm_lang$core$Native_Platform.map;
var _elm_lang$core$Platform_Sub$Sub = {ctor: 'Sub'};

var _elm_lang$core$Platform$hack = _elm_lang$core$Native_Scheduler.succeed;
var _elm_lang$core$Platform$sendToSelf = _elm_lang$core$Native_Platform.sendToSelf;
var _elm_lang$core$Platform$sendToApp = _elm_lang$core$Native_Platform.sendToApp;
var _elm_lang$core$Platform$programWithFlags = _elm_lang$core$Native_Platform.programWithFlags;
var _elm_lang$core$Platform$program = _elm_lang$core$Native_Platform.program;
var _elm_lang$core$Platform$Program = {ctor: 'Program'};
var _elm_lang$core$Platform$Task = {ctor: 'Task'};
var _elm_lang$core$Platform$ProcessId = {ctor: 'ProcessId'};
var _elm_lang$core$Platform$Router = {ctor: 'Router'};

var _elm_lang$lazy$Lazy$force = function (_p0) {
	var _p1 = _p0;
	return _p1._0(
		{ctor: '_Tuple0'});
};
var _elm_lang$lazy$Lazy$Lazy = function (a) {
	return {ctor: 'Lazy', _0: a};
};
var _elm_lang$lazy$Lazy$lazy = function (thunk) {
	return _elm_lang$lazy$Lazy$Lazy(
		_elm_lang$lazy$Native_Lazy.memoize(thunk));
};
var _elm_lang$lazy$Lazy$map = F2(
	function (f, a) {
		return _elm_lang$lazy$Lazy$lazy(
			function (_p2) {
				var _p3 = _p2;
				return f(
					_elm_lang$lazy$Lazy$force(a));
			});
	});
var _elm_lang$lazy$Lazy$map2 = F3(
	function (f, a, b) {
		return _elm_lang$lazy$Lazy$lazy(
			function (_p4) {
				var _p5 = _p4;
				return A2(
					f,
					_elm_lang$lazy$Lazy$force(a),
					_elm_lang$lazy$Lazy$force(b));
			});
	});
var _elm_lang$lazy$Lazy$map3 = F4(
	function (f, a, b, c) {
		return _elm_lang$lazy$Lazy$lazy(
			function (_p6) {
				var _p7 = _p6;
				return A3(
					f,
					_elm_lang$lazy$Lazy$force(a),
					_elm_lang$lazy$Lazy$force(b),
					_elm_lang$lazy$Lazy$force(c));
			});
	});
var _elm_lang$lazy$Lazy$map4 = F5(
	function (f, a, b, c, d) {
		return _elm_lang$lazy$Lazy$lazy(
			function (_p8) {
				var _p9 = _p8;
				return A4(
					f,
					_elm_lang$lazy$Lazy$force(a),
					_elm_lang$lazy$Lazy$force(b),
					_elm_lang$lazy$Lazy$force(c),
					_elm_lang$lazy$Lazy$force(d));
			});
	});
var _elm_lang$lazy$Lazy$map5 = F6(
	function (f, a, b, c, d, e) {
		return _elm_lang$lazy$Lazy$lazy(
			function (_p10) {
				var _p11 = _p10;
				return A5(
					f,
					_elm_lang$lazy$Lazy$force(a),
					_elm_lang$lazy$Lazy$force(b),
					_elm_lang$lazy$Lazy$force(c),
					_elm_lang$lazy$Lazy$force(d),
					_elm_lang$lazy$Lazy$force(e));
			});
	});
var _elm_lang$lazy$Lazy$apply = F2(
	function (f, x) {
		return _elm_lang$lazy$Lazy$lazy(
			function (_p12) {
				var _p13 = _p12;
				return A2(
					_elm_lang$lazy$Lazy$force,
					f,
					_elm_lang$lazy$Lazy$force(x));
			});
	});
var _elm_lang$lazy$Lazy$andThen = F2(
	function (callback, a) {
		return _elm_lang$lazy$Lazy$lazy(
			function (_p14) {
				var _p15 = _p14;
				return _elm_lang$lazy$Lazy$force(
					callback(
						_elm_lang$lazy$Lazy$force(a)));
			});
	});

//import Maybe, Native.List //

var _elm_lang$core$Native_Regex = function() {

function escape(str)
{
	return str.replace(/[-\/\\^$*+?.()|[\]{}]/g, '\\$&');
}
function caseInsensitive(re)
{
	return new RegExp(re.source, 'gi');
}
function regex(raw)
{
	return new RegExp(raw, 'g');
}

function contains(re, string)
{
	return string.match(re) !== null;
}

function find(n, re, str)
{
	n = n.ctor === 'All' ? Infinity : n._0;
	var out = [];
	var number = 0;
	var string = str;
	var lastIndex = re.lastIndex;
	var prevLastIndex = -1;
	var result;
	while (number++ < n && (result = re.exec(string)))
	{
		if (prevLastIndex === re.lastIndex) break;
		var i = result.length - 1;
		var subs = new Array(i);
		while (i > 0)
		{
			var submatch = result[i];
			subs[--i] = submatch === undefined
				? _elm_lang$core$Maybe$Nothing
				: _elm_lang$core$Maybe$Just(submatch);
		}
		out.push({
			match: result[0],
			submatches: _elm_lang$core$Native_List.fromArray(subs),
			index: result.index,
			number: number
		});
		prevLastIndex = re.lastIndex;
	}
	re.lastIndex = lastIndex;
	return _elm_lang$core$Native_List.fromArray(out);
}

function replace(n, re, replacer, string)
{
	n = n.ctor === 'All' ? Infinity : n._0;
	var count = 0;
	function jsReplacer(match)
	{
		if (count++ >= n)
		{
			return match;
		}
		var i = arguments.length - 3;
		var submatches = new Array(i);
		while (i > 0)
		{
			var submatch = arguments[i];
			submatches[--i] = submatch === undefined
				? _elm_lang$core$Maybe$Nothing
				: _elm_lang$core$Maybe$Just(submatch);
		}
		return replacer({
			match: match,
			submatches: _elm_lang$core$Native_List.fromArray(submatches),
			index: arguments[arguments.length - 2],
			number: count
		});
	}
	return string.replace(re, jsReplacer);
}

function split(n, re, str)
{
	n = n.ctor === 'All' ? Infinity : n._0;
	if (n === Infinity)
	{
		return _elm_lang$core$Native_List.fromArray(str.split(re));
	}
	var string = str;
	var result;
	var out = [];
	var start = re.lastIndex;
	var restoreLastIndex = re.lastIndex;
	while (n--)
	{
		if (!(result = re.exec(string))) break;
		out.push(string.slice(start, result.index));
		start = re.lastIndex;
	}
	out.push(string.slice(start));
	re.lastIndex = restoreLastIndex;
	return _elm_lang$core$Native_List.fromArray(out);
}

return {
	regex: regex,
	caseInsensitive: caseInsensitive,
	escape: escape,

	contains: F2(contains),
	find: F3(find),
	replace: F4(replace),
	split: F3(split)
};

}();

var _elm_lang$core$Regex$split = _elm_lang$core$Native_Regex.split;
var _elm_lang$core$Regex$replace = _elm_lang$core$Native_Regex.replace;
var _elm_lang$core$Regex$find = _elm_lang$core$Native_Regex.find;
var _elm_lang$core$Regex$contains = _elm_lang$core$Native_Regex.contains;
var _elm_lang$core$Regex$caseInsensitive = _elm_lang$core$Native_Regex.caseInsensitive;
var _elm_lang$core$Regex$regex = _elm_lang$core$Native_Regex.regex;
var _elm_lang$core$Regex$escape = _elm_lang$core$Native_Regex.escape;
var _elm_lang$core$Regex$Match = F4(
	function (a, b, c, d) {
		return {match: a, submatches: b, index: c, number: d};
	});
var _elm_lang$core$Regex$Regex = {ctor: 'Regex'};
var _elm_lang$core$Regex$AtMost = function (a) {
	return {ctor: 'AtMost', _0: a};
};
var _elm_lang$core$Regex$All = {ctor: 'All'};

var _Bogdanp$elm_combine$Combine$app = function (p) {
	var _p0 = p;
	if (_p0.ctor === 'Parser') {
		return _p0._0;
	} else {
		return _elm_lang$lazy$Lazy$force(_p0._0);
	}
};
var _Bogdanp$elm_combine$Combine$InputStream = F3(
	function (a, b, c) {
		return {data: a, input: b, position: c};
	});
var _Bogdanp$elm_combine$Combine$initStream = function (s) {
	return A3(_Bogdanp$elm_combine$Combine$InputStream, s, s, 0);
};
var _Bogdanp$elm_combine$Combine$runParser = F3(
	function (p, st, s) {
		var _p1 = A3(
			_Bogdanp$elm_combine$Combine$app,
			p,
			st,
			_Bogdanp$elm_combine$Combine$initStream(s));
		if (_p1._2.ctor === 'Ok') {
			return _elm_lang$core$Result$Ok(
				{ctor: '_Tuple3', _0: _p1._0, _1: _p1._1, _2: _p1._2._0});
		} else {
			return _elm_lang$core$Result$Err(
				{ctor: '_Tuple3', _0: _p1._0, _1: _p1._1, _2: _p1._2._0});
		}
	});
var _Bogdanp$elm_combine$Combine$parse = function (p) {
	return A2(
		_Bogdanp$elm_combine$Combine$runParser,
		p,
		{ctor: '_Tuple0'});
};
var _Bogdanp$elm_combine$Combine$ParseLocation = F3(
	function (a, b, c) {
		return {source: a, line: b, column: c};
	});
var _Bogdanp$elm_combine$Combine$currentLocation = function (stream) {
	var find = F3(
		function (position, currentLine, lines) {
			find:
			while (true) {
				var _p2 = lines;
				if (_p2.ctor === '[]') {
					return A3(_Bogdanp$elm_combine$Combine$ParseLocation, '', 1, position);
				} else {
					if (_p2._1.ctor === '[]') {
						return A3(_Bogdanp$elm_combine$Combine$ParseLocation, _p2._0, currentLine + 1, position);
					} else {
						var _p3 = _p2._0;
						var length = _elm_lang$core$String$length(_p3);
						if (_elm_lang$core$Native_Utils.cmp(position, length) > -1) {
							var _v3 = (position - length) - 1,
								_v4 = currentLine + 1,
								_v5 = _p2._1;
							position = _v3;
							currentLine = _v4;
							lines = _v5;
							continue find;
						} else {
							if (_elm_lang$core$Native_Utils.eq(currentLine, 0)) {
								return A3(_Bogdanp$elm_combine$Combine$ParseLocation, _p3, 1, position);
							} else {
								return A3(_Bogdanp$elm_combine$Combine$ParseLocation, _p3, currentLine, position - 1);
							}
						}
					}
				}
			}
		});
	var lines = A2(_elm_lang$core$String$split, '\n', stream.data);
	return A3(find, stream.position, 0, lines);
};
var _Bogdanp$elm_combine$Combine$currentSourceLine = function (_p4) {
	return function (_) {
		return _.source;
	}(
		_Bogdanp$elm_combine$Combine$currentLocation(_p4));
};
var _Bogdanp$elm_combine$Combine$currentLine = function (_p5) {
	return function (_) {
		return _.line;
	}(
		_Bogdanp$elm_combine$Combine$currentLocation(_p5));
};
var _Bogdanp$elm_combine$Combine$currentColumn = function (_p6) {
	return function (_) {
		return _.column;
	}(
		_Bogdanp$elm_combine$Combine$currentLocation(_p6));
};
var _Bogdanp$elm_combine$Combine$RecursiveParser = function (a) {
	return {ctor: 'RecursiveParser', _0: a};
};
var _Bogdanp$elm_combine$Combine$lazy = function (t) {
	return _Bogdanp$elm_combine$Combine$RecursiveParser(
		_elm_lang$lazy$Lazy$lazy(
			function (_p7) {
				var _p8 = _p7;
				return _Bogdanp$elm_combine$Combine$app(
					t(
						{ctor: '_Tuple0'}));
			}));
};
var _Bogdanp$elm_combine$Combine$Parser = function (a) {
	return {ctor: 'Parser', _0: a};
};
var _Bogdanp$elm_combine$Combine$primitive = _Bogdanp$elm_combine$Combine$Parser;
var _Bogdanp$elm_combine$Combine$bimap = F3(
	function (fok, ferr, p) {
		return _Bogdanp$elm_combine$Combine$Parser(
			F2(
				function (state, stream) {
					var _p9 = A3(_Bogdanp$elm_combine$Combine$app, p, state, stream);
					if (_p9._2.ctor === 'Ok') {
						return {
							ctor: '_Tuple3',
							_0: _p9._0,
							_1: _p9._1,
							_2: _elm_lang$core$Result$Ok(
								fok(_p9._2._0))
						};
					} else {
						return {
							ctor: '_Tuple3',
							_0: _p9._0,
							_1: _p9._1,
							_2: _elm_lang$core$Result$Err(
								ferr(_p9._2._0))
						};
					}
				}));
	});
var _Bogdanp$elm_combine$Combine$map = F2(
	function (f, p) {
		return A3(_Bogdanp$elm_combine$Combine$bimap, f, _elm_lang$core$Basics$identity, p);
	});
var _Bogdanp$elm_combine$Combine_ops = _Bogdanp$elm_combine$Combine_ops || {};
_Bogdanp$elm_combine$Combine_ops['<$>'] = _Bogdanp$elm_combine$Combine$map;
var _Bogdanp$elm_combine$Combine_ops = _Bogdanp$elm_combine$Combine_ops || {};
_Bogdanp$elm_combine$Combine_ops['<$'] = function (res) {
	return _Bogdanp$elm_combine$Combine$map(
		_elm_lang$core$Basics$always(res));
};
var _Bogdanp$elm_combine$Combine$skip = function (p) {
	return A2(
		_Bogdanp$elm_combine$Combine_ops['<$'],
		{ctor: '_Tuple0'},
		p);
};
var _Bogdanp$elm_combine$Combine_ops = _Bogdanp$elm_combine$Combine_ops || {};
_Bogdanp$elm_combine$Combine_ops['$>'] = _elm_lang$core$Basics$flip(
	F2(
		function (x, y) {
			return A2(_Bogdanp$elm_combine$Combine_ops['<$'], x, y);
		}));
var _Bogdanp$elm_combine$Combine$mapError = _Bogdanp$elm_combine$Combine$bimap(_elm_lang$core$Basics$identity);
var _Bogdanp$elm_combine$Combine_ops = _Bogdanp$elm_combine$Combine_ops || {};
_Bogdanp$elm_combine$Combine_ops['<?>'] = F2(
	function (p, m) {
		return A2(
			_Bogdanp$elm_combine$Combine$mapError,
			_elm_lang$core$Basics$always(
				{
					ctor: '::',
					_0: m,
					_1: {ctor: '[]'}
				}),
			p);
	});
var _Bogdanp$elm_combine$Combine$withState = function (f) {
	return _Bogdanp$elm_combine$Combine$Parser(
		F2(
			function (state, stream) {
				return A3(
					_Bogdanp$elm_combine$Combine$app,
					f(state),
					state,
					stream);
			}));
};
var _Bogdanp$elm_combine$Combine$withLocation = function (f) {
	return _Bogdanp$elm_combine$Combine$Parser(
		F2(
			function (state, stream) {
				return A3(
					_Bogdanp$elm_combine$Combine$app,
					f(
						_Bogdanp$elm_combine$Combine$currentLocation(stream)),
					state,
					stream);
			}));
};
var _Bogdanp$elm_combine$Combine$withLine = function (f) {
	return _Bogdanp$elm_combine$Combine$Parser(
		F2(
			function (state, stream) {
				return A3(
					_Bogdanp$elm_combine$Combine$app,
					f(
						_Bogdanp$elm_combine$Combine$currentLine(stream)),
					state,
					stream);
			}));
};
var _Bogdanp$elm_combine$Combine$withColumn = function (f) {
	return _Bogdanp$elm_combine$Combine$Parser(
		F2(
			function (state, stream) {
				return A3(
					_Bogdanp$elm_combine$Combine$app,
					f(
						_Bogdanp$elm_combine$Combine$currentColumn(stream)),
					state,
					stream);
			}));
};
var _Bogdanp$elm_combine$Combine$andThen = F2(
	function (f, p) {
		return _Bogdanp$elm_combine$Combine$Parser(
			F2(
				function (state, stream) {
					var _p10 = A3(_Bogdanp$elm_combine$Combine$app, p, state, stream);
					if (_p10._2.ctor === 'Ok') {
						return A3(
							_Bogdanp$elm_combine$Combine$app,
							f(_p10._2._0),
							_p10._0,
							_p10._1);
					} else {
						return {
							ctor: '_Tuple3',
							_0: _p10._0,
							_1: _p10._1,
							_2: _elm_lang$core$Result$Err(_p10._2._0)
						};
					}
				}));
	});
var _Bogdanp$elm_combine$Combine_ops = _Bogdanp$elm_combine$Combine_ops || {};
_Bogdanp$elm_combine$Combine_ops['>>='] = _elm_lang$core$Basics$flip(_Bogdanp$elm_combine$Combine$andThen);
var _Bogdanp$elm_combine$Combine$andMap = F2(
	function (rp, lp) {
		return A2(
			_Bogdanp$elm_combine$Combine_ops['>>='],
			lp,
			A2(_elm_lang$core$Basics$flip, _Bogdanp$elm_combine$Combine$map, rp));
	});
var _Bogdanp$elm_combine$Combine_ops = _Bogdanp$elm_combine$Combine_ops || {};
_Bogdanp$elm_combine$Combine_ops['<*>'] = _elm_lang$core$Basics$flip(_Bogdanp$elm_combine$Combine$andMap);
var _Bogdanp$elm_combine$Combine_ops = _Bogdanp$elm_combine$Combine_ops || {};
_Bogdanp$elm_combine$Combine_ops['<*'] = F2(
	function (lp, rp) {
		return A2(
			_Bogdanp$elm_combine$Combine$andMap,
			rp,
			A2(_Bogdanp$elm_combine$Combine$map, _elm_lang$core$Basics$always, lp));
	});
var _Bogdanp$elm_combine$Combine_ops = _Bogdanp$elm_combine$Combine_ops || {};
_Bogdanp$elm_combine$Combine_ops['*>'] = F2(
	function (lp, rp) {
		return A2(
			_Bogdanp$elm_combine$Combine$andMap,
			rp,
			A2(
				_Bogdanp$elm_combine$Combine$map,
				_elm_lang$core$Basics$flip(_elm_lang$core$Basics$always),
				lp));
	});
var _Bogdanp$elm_combine$Combine$between = F3(
	function (lp, rp, p) {
		return A2(
			_Bogdanp$elm_combine$Combine_ops['<*'],
			A2(_Bogdanp$elm_combine$Combine_ops['*>'], lp, p),
			rp);
	});
var _Bogdanp$elm_combine$Combine$sequence = function (ps) {
	var accumulate = F4(
		function (acc, ps, state, stream) {
			accumulate:
			while (true) {
				var _p11 = ps;
				if (_p11.ctor === '[]') {
					return {
						ctor: '_Tuple3',
						_0: state,
						_1: stream,
						_2: _elm_lang$core$Result$Ok(
							_elm_lang$core$List$reverse(acc))
					};
				} else {
					var _p12 = A3(_Bogdanp$elm_combine$Combine$app, _p11._0, state, stream);
					if (_p12._2.ctor === 'Ok') {
						var _v11 = {ctor: '::', _0: _p12._2._0, _1: acc},
							_v12 = _p11._1,
							_v13 = _p12._0,
							_v14 = _p12._1;
						acc = _v11;
						ps = _v12;
						state = _v13;
						stream = _v14;
						continue accumulate;
					} else {
						return {
							ctor: '_Tuple3',
							_0: _p12._0,
							_1: _p12._1,
							_2: _elm_lang$core$Result$Err(_p12._2._0)
						};
					}
				}
			}
		});
	return _Bogdanp$elm_combine$Combine$Parser(
		F2(
			function (state, stream) {
				return A4(
					accumulate,
					{ctor: '[]'},
					ps,
					state,
					stream);
			}));
};
var _Bogdanp$elm_combine$Combine$fail = function (m) {
	return _Bogdanp$elm_combine$Combine$Parser(
		F2(
			function (state, stream) {
				return {
					ctor: '_Tuple3',
					_0: state,
					_1: stream,
					_2: _elm_lang$core$Result$Err(
						{
							ctor: '::',
							_0: m,
							_1: {ctor: '[]'}
						})
				};
			}));
};
var _Bogdanp$elm_combine$Combine$emptyErr = _Bogdanp$elm_combine$Combine$Parser(
	F2(
		function (state, stream) {
			return {
				ctor: '_Tuple3',
				_0: state,
				_1: stream,
				_2: _elm_lang$core$Result$Err(
					{ctor: '[]'})
			};
		}));
var _Bogdanp$elm_combine$Combine$succeed = function (res) {
	return _Bogdanp$elm_combine$Combine$Parser(
		F2(
			function (state, stream) {
				return {
					ctor: '_Tuple3',
					_0: state,
					_1: stream,
					_2: _elm_lang$core$Result$Ok(res)
				};
			}));
};
var _Bogdanp$elm_combine$Combine$putState = function (state) {
	return _Bogdanp$elm_combine$Combine$Parser(
		F2(
			function (_p13, stream) {
				return A3(
					_Bogdanp$elm_combine$Combine$app,
					_Bogdanp$elm_combine$Combine$succeed(
						{ctor: '_Tuple0'}),
					state,
					stream);
			}));
};
var _Bogdanp$elm_combine$Combine$modifyState = function (f) {
	return _Bogdanp$elm_combine$Combine$Parser(
		F2(
			function (state, stream) {
				return A3(
					_Bogdanp$elm_combine$Combine$app,
					_Bogdanp$elm_combine$Combine$succeed(
						{ctor: '_Tuple0'}),
					f(state),
					stream);
			}));
};
var _Bogdanp$elm_combine$Combine$count = F2(
	function (n, p) {
		var accumulate = F2(
			function (x, acc) {
				return (_elm_lang$core$Native_Utils.cmp(x, 0) < 1) ? _Bogdanp$elm_combine$Combine$succeed(
					_elm_lang$core$List$reverse(acc)) : A2(
					_Bogdanp$elm_combine$Combine$andThen,
					function (res) {
						return A2(
							accumulate,
							x - 1,
							{ctor: '::', _0: res, _1: acc});
					},
					p);
			});
		return A2(
			accumulate,
			n,
			{ctor: '[]'});
	});
var _Bogdanp$elm_combine$Combine$string = function (s) {
	return _Bogdanp$elm_combine$Combine$Parser(
		F2(
			function (state, stream) {
				if (A2(_elm_lang$core$String$startsWith, s, stream.input)) {
					var len = _elm_lang$core$String$length(s);
					var rem = A2(_elm_lang$core$String$dropLeft, len, stream.input);
					var pos = stream.position + len;
					return {
						ctor: '_Tuple3',
						_0: state,
						_1: _elm_lang$core$Native_Utils.update(
							stream,
							{input: rem, position: pos}),
						_2: _elm_lang$core$Result$Ok(s)
					};
				} else {
					return {
						ctor: '_Tuple3',
						_0: state,
						_1: stream,
						_2: _elm_lang$core$Result$Err(
							{
								ctor: '::',
								_0: A2(
									_elm_lang$core$Basics_ops['++'],
									'expected ',
									_elm_lang$core$Basics$toString(s)),
								_1: {ctor: '[]'}
							})
					};
				}
			}));
};
var _Bogdanp$elm_combine$Combine$parens = A2(
	_Bogdanp$elm_combine$Combine$between,
	_Bogdanp$elm_combine$Combine$string('('),
	_Bogdanp$elm_combine$Combine$string(')'));
var _Bogdanp$elm_combine$Combine$braces = A2(
	_Bogdanp$elm_combine$Combine$between,
	_Bogdanp$elm_combine$Combine$string('{'),
	_Bogdanp$elm_combine$Combine$string('}'));
var _Bogdanp$elm_combine$Combine$brackets = A2(
	_Bogdanp$elm_combine$Combine$between,
	_Bogdanp$elm_combine$Combine$string('['),
	_Bogdanp$elm_combine$Combine$string(']'));
var _Bogdanp$elm_combine$Combine$regex = function (pat) {
	var pattern = A2(_elm_lang$core$String$startsWith, '^', pat) ? pat : A2(_elm_lang$core$Basics_ops['++'], '^', pat);
	return _Bogdanp$elm_combine$Combine$Parser(
		F2(
			function (state, stream) {
				var _p14 = A3(
					_elm_lang$core$Regex$find,
					_elm_lang$core$Regex$AtMost(1),
					_elm_lang$core$Regex$regex(pattern),
					stream.input);
				if ((_p14.ctor === '::') && (_p14._1.ctor === '[]')) {
					var _p15 = _p14._0;
					var len = _elm_lang$core$String$length(_p15.match);
					var rem = A2(_elm_lang$core$String$dropLeft, len, stream.input);
					var pos = stream.position + len;
					return {
						ctor: '_Tuple3',
						_0: state,
						_1: _elm_lang$core$Native_Utils.update(
							stream,
							{input: rem, position: pos}),
						_2: _elm_lang$core$Result$Ok(_p15.match)
					};
				} else {
					return {
						ctor: '_Tuple3',
						_0: state,
						_1: stream,
						_2: _elm_lang$core$Result$Err(
							{
								ctor: '::',
								_0: A2(
									_elm_lang$core$Basics_ops['++'],
									'expected input matching Regexp /',
									A2(_elm_lang$core$Basics_ops['++'], pattern, '/')),
								_1: {ctor: '[]'}
							})
					};
				}
			}));
};
var _Bogdanp$elm_combine$Combine$whitespace = A2(
	_Bogdanp$elm_combine$Combine_ops['<?>'],
	_Bogdanp$elm_combine$Combine$regex('[ \t\r\n]*'),
	'whitespace');
var _Bogdanp$elm_combine$Combine$while = function (pred) {
	var accumulate = F3(
		function (acc, state, stream) {
			accumulate:
			while (true) {
				var _p16 = _elm_lang$core$String$uncons(stream.input);
				if (_p16.ctor === 'Just') {
					var _p17 = _p16._0._0;
					if (pred(_p17)) {
						var pos = stream.position + 1;
						var c = A2(_elm_lang$core$String$cons, _p17, '');
						var _v17 = A2(_elm_lang$core$Basics_ops['++'], acc, c),
							_v18 = state,
							_v19 = _elm_lang$core$Native_Utils.update(
							stream,
							{input: _p16._0._1, position: pos});
						acc = _v17;
						state = _v18;
						stream = _v19;
						continue accumulate;
					} else {
						return {ctor: '_Tuple3', _0: state, _1: stream, _2: acc};
					}
				} else {
					return {ctor: '_Tuple3', _0: state, _1: stream, _2: acc};
				}
			}
		});
	return _Bogdanp$elm_combine$Combine$Parser(
		F2(
			function (state, stream) {
				var _p18 = A3(accumulate, '', state, stream);
				var rstate = _p18._0;
				var rstream = _p18._1;
				var res = _p18._2;
				return {
					ctor: '_Tuple3',
					_0: rstate,
					_1: rstream,
					_2: _elm_lang$core$Result$Ok(res)
				};
			}));
};
var _Bogdanp$elm_combine$Combine$end = _Bogdanp$elm_combine$Combine$Parser(
	F2(
		function (state, stream) {
			return _elm_lang$core$Native_Utils.eq(stream.input, '') ? {
				ctor: '_Tuple3',
				_0: state,
				_1: stream,
				_2: _elm_lang$core$Result$Ok(
					{ctor: '_Tuple0'})
			} : {
				ctor: '_Tuple3',
				_0: state,
				_1: stream,
				_2: _elm_lang$core$Result$Err(
					{
						ctor: '::',
						_0: 'expected end of input',
						_1: {ctor: '[]'}
					})
			};
		}));
var _Bogdanp$elm_combine$Combine$lookAhead = function (p) {
	return _Bogdanp$elm_combine$Combine$Parser(
		F2(
			function (state, stream) {
				var _p19 = A3(_Bogdanp$elm_combine$Combine$app, p, state, stream);
				if ((_p19.ctor === '_Tuple3') && (_p19._2.ctor === 'Ok')) {
					return {
						ctor: '_Tuple3',
						_0: _p19._0,
						_1: stream,
						_2: _elm_lang$core$Result$Ok(_p19._2._0)
					};
				} else {
					return _p19;
				}
			}));
};
var _Bogdanp$elm_combine$Combine$or = F2(
	function (lp, rp) {
		return _Bogdanp$elm_combine$Combine$Parser(
			F2(
				function (state, stream) {
					var _p20 = A3(_Bogdanp$elm_combine$Combine$app, lp, state, stream);
					if (_p20._2.ctor === 'Ok') {
						return _p20;
					} else {
						var _p21 = A3(_Bogdanp$elm_combine$Combine$app, rp, state, stream);
						if (_p21._2.ctor === 'Ok') {
							return _p21;
						} else {
							return {
								ctor: '_Tuple3',
								_0: state,
								_1: stream,
								_2: _elm_lang$core$Result$Err(
									A2(_elm_lang$core$Basics_ops['++'], _p20._2._0, _p21._2._0))
							};
						}
					}
				}));
	});
var _Bogdanp$elm_combine$Combine$choice = function (xs) {
	return A3(_elm_lang$core$List$foldr, _Bogdanp$elm_combine$Combine$or, _Bogdanp$elm_combine$Combine$emptyErr, xs);
};
var _Bogdanp$elm_combine$Combine_ops = _Bogdanp$elm_combine$Combine_ops || {};
_Bogdanp$elm_combine$Combine_ops['<|>'] = _Bogdanp$elm_combine$Combine$or;
var _Bogdanp$elm_combine$Combine$optional = F2(
	function (res, p) {
		return A2(
			_Bogdanp$elm_combine$Combine_ops['<|>'],
			p,
			_Bogdanp$elm_combine$Combine$succeed(res));
	});
var _Bogdanp$elm_combine$Combine$chainl = F2(
	function (op, p) {
		var accumulate = function (x) {
			return A2(
				_Bogdanp$elm_combine$Combine_ops['<|>'],
				A2(
					_Bogdanp$elm_combine$Combine$andThen,
					function (f) {
						return A2(
							_Bogdanp$elm_combine$Combine$andThen,
							function (y) {
								return accumulate(
									A2(f, x, y));
							},
							p);
					},
					op),
				_Bogdanp$elm_combine$Combine$succeed(x));
		};
		return A2(_Bogdanp$elm_combine$Combine$andThen, accumulate, p);
	});
var _Bogdanp$elm_combine$Combine$chainr = F2(
	function (op, p) {
		var accumulate = function (x) {
			return A2(
				_Bogdanp$elm_combine$Combine_ops['<|>'],
				A2(
					_Bogdanp$elm_combine$Combine$andThen,
					function (f) {
						return A2(
							_Bogdanp$elm_combine$Combine$andThen,
							function (y) {
								return _Bogdanp$elm_combine$Combine$succeed(
									A2(f, x, y));
							},
							A2(_Bogdanp$elm_combine$Combine$andThen, accumulate, p));
					},
					op),
				_Bogdanp$elm_combine$Combine$succeed(x));
		};
		return A2(_Bogdanp$elm_combine$Combine$andThen, accumulate, p);
	});
var _Bogdanp$elm_combine$Combine$maybe = function (p) {
	return _Bogdanp$elm_combine$Combine$Parser(
		F2(
			function (state, stream) {
				var _p22 = A3(_Bogdanp$elm_combine$Combine$app, p, state, stream);
				if ((_p22.ctor === '_Tuple3') && (_p22._2.ctor === 'Ok')) {
					return {
						ctor: '_Tuple3',
						_0: _p22._0,
						_1: _p22._1,
						_2: _elm_lang$core$Result$Ok(
							_elm_lang$core$Maybe$Just(_p22._2._0))
					};
				} else {
					return {
						ctor: '_Tuple3',
						_0: state,
						_1: stream,
						_2: _elm_lang$core$Result$Ok(_elm_lang$core$Maybe$Nothing)
					};
				}
			}));
};
var _Bogdanp$elm_combine$Combine$many = function (p) {
	var accumulate = F3(
		function (acc, state, stream) {
			accumulate:
			while (true) {
				var _p23 = A3(_Bogdanp$elm_combine$Combine$app, p, state, stream);
				if ((_p23.ctor === '_Tuple3') && (_p23._2.ctor === 'Ok')) {
					var _p25 = _p23._1;
					var _p24 = _p23._0;
					if (_elm_lang$core$Native_Utils.eq(stream, _p25)) {
						return {
							ctor: '_Tuple3',
							_0: _p24,
							_1: _p25,
							_2: _elm_lang$core$List$reverse(acc)
						};
					} else {
						var _v25 = {ctor: '::', _0: _p23._2._0, _1: acc},
							_v26 = _p24,
							_v27 = _p25;
						acc = _v25;
						state = _v26;
						stream = _v27;
						continue accumulate;
					}
				} else {
					return {
						ctor: '_Tuple3',
						_0: state,
						_1: stream,
						_2: _elm_lang$core$List$reverse(acc)
					};
				}
			}
		});
	return _Bogdanp$elm_combine$Combine$Parser(
		F2(
			function (state, stream) {
				var _p26 = A3(
					accumulate,
					{ctor: '[]'},
					state,
					stream);
				var rstate = _p26._0;
				var rstream = _p26._1;
				var res = _p26._2;
				return {
					ctor: '_Tuple3',
					_0: rstate,
					_1: rstream,
					_2: _elm_lang$core$Result$Ok(res)
				};
			}));
};
var _Bogdanp$elm_combine$Combine$many1 = function (p) {
	return A2(
		_Bogdanp$elm_combine$Combine_ops['<*>'],
		A2(
			_Bogdanp$elm_combine$Combine_ops['<$>'],
			F2(
				function (x, y) {
					return {ctor: '::', _0: x, _1: y};
				}),
			p),
		_Bogdanp$elm_combine$Combine$many(p));
};
var _Bogdanp$elm_combine$Combine$skipMany1 = function (p) {
	return A2(
		_Bogdanp$elm_combine$Combine_ops['<$'],
		{ctor: '_Tuple0'},
		_Bogdanp$elm_combine$Combine$many1(
			_Bogdanp$elm_combine$Combine$skip(p)));
};
var _Bogdanp$elm_combine$Combine$sepBy1 = F2(
	function (sep, p) {
		return A2(
			_Bogdanp$elm_combine$Combine_ops['<*>'],
			A2(
				_Bogdanp$elm_combine$Combine_ops['<$>'],
				F2(
					function (x, y) {
						return {ctor: '::', _0: x, _1: y};
					}),
				p),
			_Bogdanp$elm_combine$Combine$many(
				A2(_Bogdanp$elm_combine$Combine_ops['*>'], sep, p)));
	});
var _Bogdanp$elm_combine$Combine$sepBy = F2(
	function (sep, p) {
		return A2(
			_Bogdanp$elm_combine$Combine_ops['<|>'],
			A2(_Bogdanp$elm_combine$Combine$sepBy1, sep, p),
			_Bogdanp$elm_combine$Combine$succeed(
				{ctor: '[]'}));
	});
var _Bogdanp$elm_combine$Combine$sepEndBy1 = F2(
	function (sep, p) {
		return A2(
			_Bogdanp$elm_combine$Combine_ops['<*'],
			A2(_Bogdanp$elm_combine$Combine$sepBy1, sep, p),
			_Bogdanp$elm_combine$Combine$maybe(sep));
	});
var _Bogdanp$elm_combine$Combine$sepEndBy = F2(
	function (sep, p) {
		return A2(
			_Bogdanp$elm_combine$Combine_ops['<|>'],
			A2(_Bogdanp$elm_combine$Combine$sepEndBy1, sep, p),
			_Bogdanp$elm_combine$Combine$succeed(
				{ctor: '[]'}));
	});
var _Bogdanp$elm_combine$Combine$skipMany = function (p) {
	return A2(
		_Bogdanp$elm_combine$Combine_ops['<$'],
		{ctor: '_Tuple0'},
		_Bogdanp$elm_combine$Combine$many(
			_Bogdanp$elm_combine$Combine$skip(p)));
};
var _Bogdanp$elm_combine$Combine$manyTill = F2(
	function (p, end) {
		var accumulate = F3(
			function (acc, state, stream) {
				accumulate:
				while (true) {
					var _p27 = A3(_Bogdanp$elm_combine$Combine$app, end, state, stream);
					if (_p27._2.ctor === 'Ok') {
						return {
							ctor: '_Tuple3',
							_0: _p27._0,
							_1: _p27._1,
							_2: _elm_lang$core$Result$Ok(
								_elm_lang$core$List$reverse(acc))
						};
					} else {
						var _p28 = A3(_Bogdanp$elm_combine$Combine$app, p, state, stream);
						if ((_p28.ctor === '_Tuple3') && (_p28._2.ctor === 'Ok')) {
							var _v30 = {ctor: '::', _0: _p28._2._0, _1: acc},
								_v31 = _p28._0,
								_v32 = _p28._1;
							acc = _v30;
							state = _v31;
							stream = _v32;
							continue accumulate;
						} else {
							return {
								ctor: '_Tuple3',
								_0: _p27._0,
								_1: _p27._1,
								_2: _elm_lang$core$Result$Err(_p27._2._0)
							};
						}
					}
				}
			});
		return _Bogdanp$elm_combine$Combine$Parser(
			accumulate(
				{ctor: '[]'}));
	});

var _Bogdanp$elm_combine$Combine_Char$crlf = A2(
	_Bogdanp$elm_combine$Combine_ops['<$'],
	_elm_lang$core$Native_Utils.chr('\n'),
	A2(
		_Bogdanp$elm_combine$Combine_ops['<?>'],
		_Bogdanp$elm_combine$Combine$regex('\r\n'),
		'expected crlf'));
var _Bogdanp$elm_combine$Combine_Char$satisfy = function (pred) {
	return _Bogdanp$elm_combine$Combine$primitive(
		F2(
			function (state, stream) {
				var message = 'could not satisfy predicate';
				var _p0 = _elm_lang$core$String$uncons(stream.input);
				if (_p0.ctor === 'Just') {
					var _p1 = _p0._0._0;
					return pred(_p1) ? {
						ctor: '_Tuple3',
						_0: state,
						_1: _elm_lang$core$Native_Utils.update(
							stream,
							{input: _p0._0._1, position: stream.position + 1}),
						_2: _elm_lang$core$Result$Ok(_p1)
					} : {
						ctor: '_Tuple3',
						_0: state,
						_1: stream,
						_2: _elm_lang$core$Result$Err(
							{
								ctor: '::',
								_0: message,
								_1: {ctor: '[]'}
							})
					};
				} else {
					return {
						ctor: '_Tuple3',
						_0: state,
						_1: stream,
						_2: _elm_lang$core$Result$Err(
							{
								ctor: '::',
								_0: message,
								_1: {ctor: '[]'}
							})
					};
				}
			}));
};
var _Bogdanp$elm_combine$Combine_Char$char = function (c) {
	return A2(
		_Bogdanp$elm_combine$Combine_ops['<?>'],
		_Bogdanp$elm_combine$Combine_Char$satisfy(
			F2(
				function (x, y) {
					return _elm_lang$core$Native_Utils.eq(x, y);
				})(c)),
		A2(
			_elm_lang$core$Basics_ops['++'],
			'expected ',
			_elm_lang$core$Basics$toString(c)));
};
var _Bogdanp$elm_combine$Combine_Char$anyChar = A2(
	_Bogdanp$elm_combine$Combine_ops['<?>'],
	_Bogdanp$elm_combine$Combine_Char$satisfy(
		_elm_lang$core$Basics$always(true)),
	'expected any character');
var _Bogdanp$elm_combine$Combine_Char$oneOf = function (cs) {
	return A2(
		_Bogdanp$elm_combine$Combine_ops['<?>'],
		_Bogdanp$elm_combine$Combine_Char$satisfy(
			A2(_elm_lang$core$Basics$flip, _elm_lang$core$List$member, cs)),
		A2(
			_elm_lang$core$Basics_ops['++'],
			'expected one of ',
			_elm_lang$core$Basics$toString(cs)));
};
var _Bogdanp$elm_combine$Combine_Char$noneOf = function (cs) {
	return A2(
		_Bogdanp$elm_combine$Combine_ops['<?>'],
		_Bogdanp$elm_combine$Combine_Char$satisfy(
			function (_p2) {
				return !A3(_elm_lang$core$Basics$flip, _elm_lang$core$List$member, cs, _p2);
			}),
		A2(
			_elm_lang$core$Basics_ops['++'],
			'expected none of ',
			_elm_lang$core$Basics$toString(cs)));
};
var _Bogdanp$elm_combine$Combine_Char$space = A2(
	_Bogdanp$elm_combine$Combine_ops['<?>'],
	_Bogdanp$elm_combine$Combine_Char$satisfy(
		F2(
			function (x, y) {
				return _elm_lang$core$Native_Utils.eq(x, y);
			})(
			_elm_lang$core$Native_Utils.chr(' '))),
	'expected space');
var _Bogdanp$elm_combine$Combine_Char$tab = A2(
	_Bogdanp$elm_combine$Combine_ops['<?>'],
	_Bogdanp$elm_combine$Combine_Char$satisfy(
		F2(
			function (x, y) {
				return _elm_lang$core$Native_Utils.eq(x, y);
			})(
			_elm_lang$core$Native_Utils.chr('\t'))),
	'expected tab');
var _Bogdanp$elm_combine$Combine_Char$newline = A2(
	_Bogdanp$elm_combine$Combine_ops['<?>'],
	_Bogdanp$elm_combine$Combine_Char$satisfy(
		F2(
			function (x, y) {
				return _elm_lang$core$Native_Utils.eq(x, y);
			})(
			_elm_lang$core$Native_Utils.chr('\n'))),
	'expected newline');
var _Bogdanp$elm_combine$Combine_Char$eol = A2(_Bogdanp$elm_combine$Combine_ops['<|>'], _Bogdanp$elm_combine$Combine_Char$newline, _Bogdanp$elm_combine$Combine_Char$crlf);
var _Bogdanp$elm_combine$Combine_Char$lower = A2(
	_Bogdanp$elm_combine$Combine_ops['<?>'],
	_Bogdanp$elm_combine$Combine_Char$satisfy(_elm_lang$core$Char$isLower),
	'expected a lowercase character');
var _Bogdanp$elm_combine$Combine_Char$upper = A2(
	_Bogdanp$elm_combine$Combine_ops['<?>'],
	_Bogdanp$elm_combine$Combine_Char$satisfy(_elm_lang$core$Char$isUpper),
	'expected an uppercase character');
var _Bogdanp$elm_combine$Combine_Char$digit = A2(
	_Bogdanp$elm_combine$Combine_ops['<?>'],
	_Bogdanp$elm_combine$Combine_Char$satisfy(_elm_lang$core$Char$isDigit),
	'expected a digit');
var _Bogdanp$elm_combine$Combine_Char$octDigit = A2(
	_Bogdanp$elm_combine$Combine_ops['<?>'],
	_Bogdanp$elm_combine$Combine_Char$satisfy(_elm_lang$core$Char$isOctDigit),
	'expected an octal digit');
var _Bogdanp$elm_combine$Combine_Char$hexDigit = A2(
	_Bogdanp$elm_combine$Combine_ops['<?>'],
	_Bogdanp$elm_combine$Combine_Char$satisfy(_elm_lang$core$Char$isHexDigit),
	'expected a hexadecimal digit');

var _Bogdanp$elm_combine$Combine_Num$digit = function () {
	var toDigit = function (c) {
		return _elm_lang$core$Char$toCode(c) - _elm_lang$core$Char$toCode(
			_elm_lang$core$Native_Utils.chr('0'));
	};
	return A2(
		_Bogdanp$elm_combine$Combine_ops['<$>'],
		toDigit,
		A2(_Bogdanp$elm_combine$Combine_ops['<?>'], _Bogdanp$elm_combine$Combine_Char$digit, 'expected a digit'));
}();
var _Bogdanp$elm_combine$Combine_Num$sign = A2(
	_Bogdanp$elm_combine$Combine$optional,
	1,
	_Bogdanp$elm_combine$Combine$choice(
		{
			ctor: '::',
			_0: A2(
				_Bogdanp$elm_combine$Combine_ops['<$'],
				1,
				_Bogdanp$elm_combine$Combine$string('+')),
			_1: {
				ctor: '::',
				_0: A2(
					_Bogdanp$elm_combine$Combine_ops['<$'],
					-1,
					_Bogdanp$elm_combine$Combine$string('-')),
				_1: {ctor: '[]'}
			}
		}));
var _Bogdanp$elm_combine$Combine_Num$unwrap = F2(
	function (f, s) {
		var _p0 = f(s);
		if (_p0.ctor === 'Ok') {
			return _p0._0;
		} else {
			return _elm_lang$core$Native_Utils.crashCase(
				'Combine.Num',
				{
					start: {line: 23, column: 3},
					end: {line: 28, column: 79}
				},
				_p0)(
				A2(
					_elm_lang$core$Basics_ops['++'],
					'impossible state in Combine.Num.unwrap: ',
					_elm_lang$core$Basics$toString(_p0._0)));
		}
	});
var _Bogdanp$elm_combine$Combine_Num$toInt = _Bogdanp$elm_combine$Combine_Num$unwrap(_elm_lang$core$String$toInt);
var _Bogdanp$elm_combine$Combine_Num$int = A2(
	_Bogdanp$elm_combine$Combine_ops['<*>'],
	A2(
		_Bogdanp$elm_combine$Combine_ops['<$>'],
		F2(
			function (x, y) {
				return x * y;
			}),
		_Bogdanp$elm_combine$Combine_Num$sign),
	A2(
		_Bogdanp$elm_combine$Combine_ops['<?>'],
		A2(
			_Bogdanp$elm_combine$Combine_ops['<$>'],
			_Bogdanp$elm_combine$Combine_Num$toInt,
			_Bogdanp$elm_combine$Combine$regex('(0|[1-9][0-9]*)')),
		'expected an integer'));
var _Bogdanp$elm_combine$Combine_Num$toFloat = _Bogdanp$elm_combine$Combine_Num$unwrap(_elm_lang$core$String$toFloat);
var _Bogdanp$elm_combine$Combine_Num$float = A2(
	_Bogdanp$elm_combine$Combine_ops['<*>'],
	A2(
		_Bogdanp$elm_combine$Combine_ops['<$>'],
		function (_p2) {
			return F2(
				function (x, y) {
					return x * y;
				})(
				_elm_lang$core$Basics$toFloat(_p2));
		},
		_Bogdanp$elm_combine$Combine_Num$sign),
	A2(
		_Bogdanp$elm_combine$Combine_ops['<?>'],
		A2(
			_Bogdanp$elm_combine$Combine_ops['<$>'],
			_Bogdanp$elm_combine$Combine_Num$toFloat,
			_Bogdanp$elm_combine$Combine$regex('(0|[1-9][0-9]*)(\\.[0-9]+)')),
		'expected a float'));

var _Bogdanp$elm_time$Time_Internal$paddedInt = A2(
	_Bogdanp$elm_combine$Combine_ops['*>'],
	A2(
		_Bogdanp$elm_combine$Combine$optional,
		'',
		_Bogdanp$elm_combine$Combine$string('0')),
	_Bogdanp$elm_combine$Combine_Num$int);
var _Bogdanp$elm_time$Time_Internal$intRange = F2(
	function (lo, hi) {
		var validate = function (n) {
			return ((_elm_lang$core$Native_Utils.cmp(n, lo) > -1) && (_elm_lang$core$Native_Utils.cmp(n, hi) < 1)) ? _Bogdanp$elm_combine$Combine$succeed(n) : _Bogdanp$elm_combine$Combine$fail(
				A2(
					_elm_lang$core$Basics_ops['++'],
					'expected an integer in the range [',
					A2(
						_elm_lang$core$Basics_ops['++'],
						_elm_lang$core$Basics$toString(lo),
						A2(
							_elm_lang$core$Basics_ops['++'],
							', ',
							A2(
								_elm_lang$core$Basics_ops['++'],
								_elm_lang$core$Basics$toString(hi),
								']')))));
		};
		return A2(_Bogdanp$elm_combine$Combine_ops['>>='], _Bogdanp$elm_time$Time_Internal$paddedInt, validate);
	});
var _Bogdanp$elm_time$Time_Internal$secondMs = 1000;
var _Bogdanp$elm_time$Time_Internal$minuteMs = 60000;
var _Bogdanp$elm_time$Time_Internal$hourMs = 3600000;
var _Bogdanp$elm_time$Time_Internal$dayMs = 86400000;
var _Bogdanp$elm_time$Time_Internal$padded = function (n) {
	return (_elm_lang$core$Native_Utils.cmp(n, 10) < 0) ? A2(
		_elm_lang$core$Basics_ops['++'],
		'0',
		_elm_lang$core$Basics$toString(n)) : _elm_lang$core$Basics$toString(n);
};
var _Bogdanp$elm_time$Time_Internal$zero = {year: 0, month: 1, day: 1, hour: 0, minute: 0, second: 0, millisecond: 0};
var _Bogdanp$elm_time$Time_Internal$offsetFromTimeData = function (_p0) {
	var _p1 = _p0;
	return (((A3(_elm_lang$core$Basics$clamp, 0, 23, _p1.hour) * _Bogdanp$elm_time$Time_Internal$hourMs) + (A3(_elm_lang$core$Basics$clamp, 0, 59, _p1.minute) * _Bogdanp$elm_time$Time_Internal$minuteMs)) + (A3(_elm_lang$core$Basics$clamp, 0, 59, _p1.second) * _Bogdanp$elm_time$Time_Internal$secondMs)) + A3(_elm_lang$core$Basics$clamp, 0, 999, _p1.millisecond);
};
var _Bogdanp$elm_time$Time_Internal$DateTimeData = F7(
	function (a, b, c, d, e, f, g) {
		return {year: a, month: b, day: c, hour: d, minute: e, second: f, millisecond: g};
	});

var _Bogdanp$elm_time$Time_Date$clampDay = function (day) {
	return A3(_elm_lang$core$Basics$clamp, 1, 31, day);
};
var _Bogdanp$elm_time$Time_Date$clampMonth = function (month) {
	return A3(_elm_lang$core$Basics$clamp, 1, 12, month);
};
var _Bogdanp$elm_time$Time_Date$daysFromYear = function (y) {
	return (_elm_lang$core$Native_Utils.cmp(y, 0) > 0) ? ((((366 + ((y - 1) * 365)) + (((y - 1) / 4) | 0)) - (((y - 1) / 100) | 0)) + (((y - 1) / 400) | 0)) : ((_elm_lang$core$Native_Utils.cmp(y, 0) < 0) ? ((((y * 365) + ((y / 4) | 0)) - ((y / 100) | 0)) + ((y / 400) | 0)) : 0);
};
var _Bogdanp$elm_time$Time_Date$yearFromDays = function (ds) {
	var y = (ds / 365) | 0;
	var d = _Bogdanp$elm_time$Time_Date$daysFromYear(y);
	return (_elm_lang$core$Native_Utils.cmp(ds, d) < 1) ? (y - 1) : y;
};
var _Bogdanp$elm_time$Time_Date$isLeapYear = function (y) {
	return _elm_lang$core$Native_Utils.eq(
		A2(_elm_lang$core$Basics_ops['%'], y, 400),
		0) || ((!_elm_lang$core$Native_Utils.eq(
		A2(_elm_lang$core$Basics_ops['%'], y, 100),
		0)) && _elm_lang$core$Native_Utils.eq(
		A2(_elm_lang$core$Basics_ops['%'], y, 4),
		0));
};
var _Bogdanp$elm_time$Time_Date$unsafeDaysInMonth = F2(
	function (y, m) {
		return _elm_lang$core$Native_Utils.eq(m, 1) ? 31 : ((_elm_lang$core$Native_Utils.eq(m, 2) && _Bogdanp$elm_time$Time_Date$isLeapYear(y)) ? 29 : (_elm_lang$core$Native_Utils.eq(m, 2) ? 28 : (_elm_lang$core$Native_Utils.eq(m, 3) ? 31 : (_elm_lang$core$Native_Utils.eq(m, 4) ? 30 : (_elm_lang$core$Native_Utils.eq(m, 5) ? 31 : (_elm_lang$core$Native_Utils.eq(m, 6) ? 30 : (_elm_lang$core$Native_Utils.eq(m, 7) ? 31 : (_elm_lang$core$Native_Utils.eq(m, 8) ? 31 : (_elm_lang$core$Native_Utils.eq(m, 9) ? 30 : (_elm_lang$core$Native_Utils.eq(m, 10) ? 31 : (_elm_lang$core$Native_Utils.eq(m, 11) ? 30 : (_elm_lang$core$Native_Utils.eq(m, 12) ? 31 : _elm_lang$core$Native_Utils.crash(
			'Time.Date',
			{
				start: {line: 343, column: 9},
				end: {line: 343, column: 20}
			})(
			A2(
				_elm_lang$core$Basics_ops['++'],
				'invalid call to unsafeDaysInMonth: year=',
				A2(
					_elm_lang$core$Basics_ops['++'],
					_elm_lang$core$Basics$toString(y),
					A2(
						_elm_lang$core$Basics_ops['++'],
						' month=',
						_elm_lang$core$Basics$toString(m)))))))))))))))));
	});
var _Bogdanp$elm_time$Time_Date$daysInMonth = F2(
	function (y, m) {
		return ((_elm_lang$core$Native_Utils.cmp(m, 1) > -1) && (_elm_lang$core$Native_Utils.cmp(m, 12) < 1)) ? _elm_lang$core$Maybe$Just(
			A2(_Bogdanp$elm_time$Time_Date$unsafeDaysInMonth, y, m)) : _elm_lang$core$Maybe$Nothing;
	});
var _Bogdanp$elm_time$Time_Date$daysFromYearMonth = F2(
	function (year, month) {
		var go = F3(
			function (year, month, acc) {
				go:
				while (true) {
					if (_elm_lang$core$Native_Utils.eq(month, 0)) {
						return acc;
					} else {
						var _v0 = year,
							_v1 = month - 1,
							_v2 = acc + A2(_Bogdanp$elm_time$Time_Date$unsafeDaysInMonth, year, month);
						year = _v0;
						month = _v1;
						acc = _v2;
						continue go;
					}
				}
			});
		return A3(go, year, month - 1, 0);
	});
var _Bogdanp$elm_time$Time_Date$daysFromYearMonthDay = F3(
	function (year, month, day) {
		var dds = day - 1;
		var mds = A2(_Bogdanp$elm_time$Time_Date$daysFromYearMonth, year, month);
		var yds = _Bogdanp$elm_time$Time_Date$daysFromYear(year);
		return (yds + mds) + dds;
	});
var _Bogdanp$elm_time$Time_Date$isValidDate = F3(
	function (year, month, day) {
		return A2(
			_elm_lang$core$Maybe$withDefault,
			false,
			A2(
				_elm_lang$core$Maybe$map,
				function (days) {
					return (_elm_lang$core$Native_Utils.cmp(day, 1) > -1) && (_elm_lang$core$Native_Utils.cmp(day, days) < 1);
				},
				A2(_Bogdanp$elm_time$Time_Date$daysInMonth, year, month)));
	});
var _Bogdanp$elm_time$Time_Date$toTuple = function (_p0) {
	var _p1 = _p0;
	return {ctor: '_Tuple3', _0: _p1._0.year, _1: _p1._0.month, _2: _p1._0.day};
};
var _Bogdanp$elm_time$Time_Date$delta = F2(
	function (_p3, _p2) {
		var _p4 = _p3;
		var _p7 = _p4._0;
		var _p5 = _p2;
		var _p6 = _p5._0;
		return {
			years: _p7.year - _p6.year,
			months: ((_elm_lang$core$Basics$abs(_p7.year) * 12) + _p7.month) - ((_elm_lang$core$Basics$abs(_p6.year) * 12) + _p6.month),
			days: A3(_Bogdanp$elm_time$Time_Date$daysFromYearMonthDay, _p7.year, _p7.month, _p7.day) - A3(_Bogdanp$elm_time$Time_Date$daysFromYearMonthDay, _p6.year, _p6.month, _p6.day)
		};
	});
var _Bogdanp$elm_time$Time_Date$compare = F2(
	function (d1, d2) {
		return A2(
			_elm_lang$core$Basics$compare,
			_Bogdanp$elm_time$Time_Date$toTuple(d1),
			_Bogdanp$elm_time$Time_Date$toTuple(d2));
	});
var _Bogdanp$elm_time$Time_Date$day = function (_p8) {
	var _p9 = _p8;
	return _p9._0.day;
};
var _Bogdanp$elm_time$Time_Date$month = function (_p10) {
	var _p11 = _p10;
	return _p11._0.month;
};
var _Bogdanp$elm_time$Time_Date$year = function (_p12) {
	var _p13 = _p12;
	return _p13._0.year;
};
var _Bogdanp$elm_time$Time_Date$toISO8601 = function (d) {
	return A2(
		_elm_lang$core$Basics_ops['++'],
		_elm_lang$core$Basics$toString(
			_Bogdanp$elm_time$Time_Date$year(d)),
		A2(
			_elm_lang$core$Basics_ops['++'],
			'-',
			A2(
				_elm_lang$core$Basics_ops['++'],
				_Bogdanp$elm_time$Time_Internal$padded(
					_Bogdanp$elm_time$Time_Date$month(d)),
				A2(
					_elm_lang$core$Basics_ops['++'],
					'-',
					_Bogdanp$elm_time$Time_Internal$padded(
						_Bogdanp$elm_time$Time_Date$day(d))))));
};
var _Bogdanp$elm_time$Time_Date$DateDelta = F3(
	function (a, b, c) {
		return {years: a, months: b, days: c};
	});
var _Bogdanp$elm_time$Time_Date$Date = function (a) {
	return {ctor: 'Date', _0: a};
};
var _Bogdanp$elm_time$Time_Date$firstValid = F3(
	function (year, month, day) {
		var _p14 = A3(_Bogdanp$elm_time$Time_Date$isValidDate, year, month, day) ? {ctor: '_Tuple3', _0: year, _1: month, _2: day} : (A3(_Bogdanp$elm_time$Time_Date$isValidDate, year, month, day - 1) ? {ctor: '_Tuple3', _0: year, _1: month, _2: day - 1} : (A3(_Bogdanp$elm_time$Time_Date$isValidDate, year, month, day - 2) ? {ctor: '_Tuple3', _0: year, _1: month, _2: day - 2} : {ctor: '_Tuple3', _0: year, _1: month, _2: day - 3}));
		var y = _p14._0;
		var m = _p14._1;
		var d = _p14._2;
		return _Bogdanp$elm_time$Time_Date$Date(
			{year: y, month: m, day: d});
	});
var _Bogdanp$elm_time$Time_Date$date = F3(
	function (year, month, day) {
		return A3(
			_Bogdanp$elm_time$Time_Date$firstValid,
			year,
			_Bogdanp$elm_time$Time_Date$clampMonth(month),
			_Bogdanp$elm_time$Time_Date$clampDay(day));
	});
var _Bogdanp$elm_time$Time_Date$fromTuple = function (_p15) {
	var _p16 = _p15;
	return A3(_Bogdanp$elm_time$Time_Date$date, _p16._0, _p16._1, _p16._2);
};
var _Bogdanp$elm_time$Time_Date$fromISO8601 = function (input) {
	var convert = function (_p17) {
		var _p18 = _p17;
		var _p21 = _p18._0;
		var _p20 = _p18._1;
		var _p19 = _p18._2;
		return A3(_Bogdanp$elm_time$Time_Date$isValidDate, _p21, _p20, _p19) ? _Bogdanp$elm_combine$Combine$succeed(
			A3(_Bogdanp$elm_time$Time_Date$date, _p21, _p20, _p19)) : _Bogdanp$elm_combine$Combine$fail('invalid date');
	};
	var dateTuple = A2(
		_Bogdanp$elm_combine$Combine_ops['<*>'],
		A2(
			_Bogdanp$elm_combine$Combine_ops['<*>'],
			A2(
				_Bogdanp$elm_combine$Combine_ops['<$>'],
				F3(
					function (v0, v1, v2) {
						return {ctor: '_Tuple3', _0: v0, _1: v1, _2: v2};
					}),
				_Bogdanp$elm_combine$Combine_Num$int),
			A2(
				_Bogdanp$elm_combine$Combine_ops['*>'],
				_Bogdanp$elm_combine$Combine$string('-'),
				A2(_Bogdanp$elm_time$Time_Internal$intRange, 1, 12))),
		A2(
			_Bogdanp$elm_combine$Combine_ops['*>'],
			_Bogdanp$elm_combine$Combine$string('-'),
			A2(_Bogdanp$elm_time$Time_Internal$intRange, 1, 31)));
	var _p22 = A2(
		_Bogdanp$elm_combine$Combine$parse,
		A2(_Bogdanp$elm_combine$Combine_ops['>>='], dateTuple, convert),
		input);
	if (_p22.ctor === 'Ok') {
		return _elm_lang$core$Result$Ok(_p22._0._2);
	} else {
		var messages = A2(_elm_lang$core$String$join, ' or ', _p22._0._2);
		return _elm_lang$core$Result$Err(
			A2(
				_elm_lang$core$Basics_ops['++'],
				'Errors encountered at position ',
				A2(
					_elm_lang$core$Basics_ops['++'],
					_elm_lang$core$Basics$toString(_p22._0._1.position),
					A2(_elm_lang$core$Basics_ops['++'], ': ', messages))));
	}
};
var _Bogdanp$elm_time$Time_Date$setYear = F2(
	function (year, _p23) {
		var _p24 = _p23;
		return A3(_Bogdanp$elm_time$Time_Date$firstValid, year, _p24._0.month, _p24._0.day);
	});
var _Bogdanp$elm_time$Time_Date$setMonth = F2(
	function (month, _p25) {
		var _p26 = _p25;
		return A3(
			_Bogdanp$elm_time$Time_Date$firstValid,
			_p26._0.year,
			_Bogdanp$elm_time$Time_Date$clampMonth(month),
			_p26._0.day);
	});
var _Bogdanp$elm_time$Time_Date$setDay = F2(
	function (day, _p27) {
		var _p28 = _p27;
		return A3(
			_Bogdanp$elm_time$Time_Date$firstValid,
			_p28._0.year,
			_p28._0.month,
			_Bogdanp$elm_time$Time_Date$clampDay(day));
	});
var _Bogdanp$elm_time$Time_Date$addYears = F2(
	function (years, _p29) {
		var _p30 = _p29;
		return A3(_Bogdanp$elm_time$Time_Date$firstValid, _p30._0.year + years, _p30._0.month, _p30._0.day);
	});
var _Bogdanp$elm_time$Time_Date$addMonths = F2(
	function (months, _p31) {
		var _p32 = _p31;
		var _p33 = _p32._0.year;
		var ms = (((_elm_lang$core$Basics$abs(_p33) * 12) + _p32._0.month) - 1) + months;
		var sign = (_elm_lang$core$Native_Utils.cmp(_p33, 0) < 0) ? -1 : 1;
		return A3(
			_Bogdanp$elm_time$Time_Date$firstValid,
			((sign * ms) / 12) | 0,
			A2(_elm_lang$core$Basics_ops['%'], ms, 12) + 1,
			_p32._0.day);
	});
var _Bogdanp$elm_time$Time_Date$dateFromDays = function (ds) {
	var d400 = _Bogdanp$elm_time$Time_Date$daysFromYear(400);
	var y400 = (ds / d400) | 0;
	var d = A2(_elm_lang$core$Basics$rem, ds, d400);
	var year = _Bogdanp$elm_time$Time_Date$yearFromDays(d + 1);
	var leap = _Bogdanp$elm_time$Time_Date$isLeapYear(year) ? F2(
		function (x, y) {
			return x + y;
		})(1) : _elm_lang$core$Basics$identity;
	var doy = d - _Bogdanp$elm_time$Time_Date$daysFromYear(year);
	var _p34 = (_elm_lang$core$Native_Utils.cmp(doy, 31) < 0) ? {ctor: '_Tuple2', _0: 1, _1: doy + 1} : ((_elm_lang$core$Native_Utils.cmp(
		doy,
		leap(59)) < 0) ? {ctor: '_Tuple2', _0: 2, _1: (doy - 31) + 1} : ((_elm_lang$core$Native_Utils.cmp(
		doy,
		leap(90)) < 0) ? {
		ctor: '_Tuple2',
		_0: 3,
		_1: (doy - leap(59)) + 1
	} : ((_elm_lang$core$Native_Utils.cmp(
		doy,
		leap(120)) < 0) ? {
		ctor: '_Tuple2',
		_0: 4,
		_1: (doy - leap(90)) + 1
	} : ((_elm_lang$core$Native_Utils.cmp(
		doy,
		leap(151)) < 0) ? {
		ctor: '_Tuple2',
		_0: 5,
		_1: (doy - leap(120)) + 1
	} : ((_elm_lang$core$Native_Utils.cmp(
		doy,
		leap(181)) < 0) ? {
		ctor: '_Tuple2',
		_0: 6,
		_1: (doy - leap(151)) + 1
	} : ((_elm_lang$core$Native_Utils.cmp(
		doy,
		leap(212)) < 0) ? {
		ctor: '_Tuple2',
		_0: 7,
		_1: (doy - leap(181)) + 1
	} : ((_elm_lang$core$Native_Utils.cmp(
		doy,
		leap(243)) < 0) ? {
		ctor: '_Tuple2',
		_0: 8,
		_1: (doy - leap(212)) + 1
	} : ((_elm_lang$core$Native_Utils.cmp(
		doy,
		leap(273)) < 0) ? {
		ctor: '_Tuple2',
		_0: 9,
		_1: (doy - leap(243)) + 1
	} : ((_elm_lang$core$Native_Utils.cmp(
		doy,
		leap(304)) < 0) ? {
		ctor: '_Tuple2',
		_0: 10,
		_1: (doy - leap(273)) + 1
	} : ((_elm_lang$core$Native_Utils.cmp(
		doy,
		leap(334)) < 0) ? {
		ctor: '_Tuple2',
		_0: 11,
		_1: (doy - leap(304)) + 1
	} : {
		ctor: '_Tuple2',
		_0: 12,
		_1: (doy - leap(334)) + 1
	}))))))))));
	var month = _p34._0;
	var day = _p34._1;
	return _Bogdanp$elm_time$Time_Date$Date(
		{year: year + (y400 * 400), month: month, day: day});
};
var _Bogdanp$elm_time$Time_Date$addDays = F2(
	function (days, _p35) {
		var _p36 = _p35;
		return _Bogdanp$elm_time$Time_Date$dateFromDays(
			A2(
				F2(
					function (x, y) {
						return x + y;
					}),
				days,
				A3(_Bogdanp$elm_time$Time_Date$daysFromYearMonthDay, _p36._0.year, _p36._0.month, _p36._0.day)));
	});
var _Bogdanp$elm_time$Time_Date$Sun = {ctor: 'Sun'};
var _Bogdanp$elm_time$Time_Date$Sat = {ctor: 'Sat'};
var _Bogdanp$elm_time$Time_Date$Fri = {ctor: 'Fri'};
var _Bogdanp$elm_time$Time_Date$Thu = {ctor: 'Thu'};
var _Bogdanp$elm_time$Time_Date$Wed = {ctor: 'Wed'};
var _Bogdanp$elm_time$Time_Date$Tue = {ctor: 'Tue'};
var _Bogdanp$elm_time$Time_Date$Mon = {ctor: 'Mon'};
var _Bogdanp$elm_time$Time_Date$weekday = function (_p37) {
	var _p38 = _p37;
	var _p40 = _p38._0.year;
	var _p39 = _p38._0.month;
	var y = (_elm_lang$core$Native_Utils.cmp(_p39, 3) < 0) ? (_p40 - 1) : _p40;
	var m = _elm_lang$core$Native_Utils.eq(_p39, 1) ? 0 : (_elm_lang$core$Native_Utils.eq(_p39, 2) ? 3 : (_elm_lang$core$Native_Utils.eq(_p39, 3) ? 2 : (_elm_lang$core$Native_Utils.eq(_p39, 4) ? 5 : (_elm_lang$core$Native_Utils.eq(_p39, 5) ? 0 : (_elm_lang$core$Native_Utils.eq(_p39, 6) ? 3 : (_elm_lang$core$Native_Utils.eq(_p39, 7) ? 5 : (_elm_lang$core$Native_Utils.eq(_p39, 8) ? 1 : (_elm_lang$core$Native_Utils.eq(_p39, 9) ? 4 : (_elm_lang$core$Native_Utils.eq(_p39, 10) ? 6 : (_elm_lang$core$Native_Utils.eq(_p39, 11) ? 2 : 4))))))))));
	var d = A2(_elm_lang$core$Basics_ops['%'], ((((y + ((y / 4) | 0)) - ((y / 100) | 0)) + ((y / 400) | 0)) + m) + _p38._0.day, 7);
	return _elm_lang$core$Native_Utils.eq(d, 0) ? _Bogdanp$elm_time$Time_Date$Sun : (_elm_lang$core$Native_Utils.eq(d, 1) ? _Bogdanp$elm_time$Time_Date$Mon : (_elm_lang$core$Native_Utils.eq(d, 2) ? _Bogdanp$elm_time$Time_Date$Tue : (_elm_lang$core$Native_Utils.eq(d, 3) ? _Bogdanp$elm_time$Time_Date$Wed : (_elm_lang$core$Native_Utils.eq(d, 4) ? _Bogdanp$elm_time$Time_Date$Thu : (_elm_lang$core$Native_Utils.eq(d, 5) ? _Bogdanp$elm_time$Time_Date$Fri : _Bogdanp$elm_time$Time_Date$Sat)))));
};

var _elm_lang$core$Dict$foldr = F3(
	function (f, acc, t) {
		foldr:
		while (true) {
			var _p0 = t;
			if (_p0.ctor === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var _v1 = f,
					_v2 = A3(
					f,
					_p0._1,
					_p0._2,
					A3(_elm_lang$core$Dict$foldr, f, acc, _p0._4)),
					_v3 = _p0._3;
				f = _v1;
				acc = _v2;
				t = _v3;
				continue foldr;
			}
		}
	});
var _elm_lang$core$Dict$keys = function (dict) {
	return A3(
		_elm_lang$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return {ctor: '::', _0: key, _1: keyList};
			}),
		{ctor: '[]'},
		dict);
};
var _elm_lang$core$Dict$values = function (dict) {
	return A3(
		_elm_lang$core$Dict$foldr,
		F3(
			function (key, value, valueList) {
				return {ctor: '::', _0: value, _1: valueList};
			}),
		{ctor: '[]'},
		dict);
};
var _elm_lang$core$Dict$toList = function (dict) {
	return A3(
		_elm_lang$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return {
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: key, _1: value},
					_1: list
				};
			}),
		{ctor: '[]'},
		dict);
};
var _elm_lang$core$Dict$foldl = F3(
	function (f, acc, dict) {
		foldl:
		while (true) {
			var _p1 = dict;
			if (_p1.ctor === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var _v5 = f,
					_v6 = A3(
					f,
					_p1._1,
					_p1._2,
					A3(_elm_lang$core$Dict$foldl, f, acc, _p1._3)),
					_v7 = _p1._4;
				f = _v5;
				acc = _v6;
				dict = _v7;
				continue foldl;
			}
		}
	});
var _elm_lang$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _p2) {
				stepState:
				while (true) {
					var _p3 = _p2;
					var _p9 = _p3._1;
					var _p8 = _p3._0;
					var _p4 = _p8;
					if (_p4.ctor === '[]') {
						return {
							ctor: '_Tuple2',
							_0: _p8,
							_1: A3(rightStep, rKey, rValue, _p9)
						};
					} else {
						var _p7 = _p4._1;
						var _p6 = _p4._0._1;
						var _p5 = _p4._0._0;
						if (_elm_lang$core$Native_Utils.cmp(_p5, rKey) < 0) {
							var _v10 = rKey,
								_v11 = rValue,
								_v12 = {
								ctor: '_Tuple2',
								_0: _p7,
								_1: A3(leftStep, _p5, _p6, _p9)
							};
							rKey = _v10;
							rValue = _v11;
							_p2 = _v12;
							continue stepState;
						} else {
							if (_elm_lang$core$Native_Utils.cmp(_p5, rKey) > 0) {
								return {
									ctor: '_Tuple2',
									_0: _p8,
									_1: A3(rightStep, rKey, rValue, _p9)
								};
							} else {
								return {
									ctor: '_Tuple2',
									_0: _p7,
									_1: A4(bothStep, _p5, _p6, rValue, _p9)
								};
							}
						}
					}
				}
			});
		var _p10 = A3(
			_elm_lang$core$Dict$foldl,
			stepState,
			{
				ctor: '_Tuple2',
				_0: _elm_lang$core$Dict$toList(leftDict),
				_1: initialResult
			},
			rightDict);
		var leftovers = _p10._0;
		var intermediateResult = _p10._1;
		return A3(
			_elm_lang$core$List$foldl,
			F2(
				function (_p11, result) {
					var _p12 = _p11;
					return A3(leftStep, _p12._0, _p12._1, result);
				}),
			intermediateResult,
			leftovers);
	});
var _elm_lang$core$Dict$reportRemBug = F4(
	function (msg, c, lgot, rgot) {
		return _elm_lang$core$Native_Debug.crash(
			_elm_lang$core$String$concat(
				{
					ctor: '::',
					_0: 'Internal red-black tree invariant violated, expected ',
					_1: {
						ctor: '::',
						_0: msg,
						_1: {
							ctor: '::',
							_0: ' and got ',
							_1: {
								ctor: '::',
								_0: _elm_lang$core$Basics$toString(c),
								_1: {
									ctor: '::',
									_0: '/',
									_1: {
										ctor: '::',
										_0: lgot,
										_1: {
											ctor: '::',
											_0: '/',
											_1: {
												ctor: '::',
												_0: rgot,
												_1: {
													ctor: '::',
													_0: '\nPlease report this bug to <https://github.com/elm-lang/core/issues>',
													_1: {ctor: '[]'}
												}
											}
										}
									}
								}
							}
						}
					}
				}));
	});
var _elm_lang$core$Dict$isBBlack = function (dict) {
	var _p13 = dict;
	_v14_2:
	do {
		if (_p13.ctor === 'RBNode_elm_builtin') {
			if (_p13._0.ctor === 'BBlack') {
				return true;
			} else {
				break _v14_2;
			}
		} else {
			if (_p13._0.ctor === 'LBBlack') {
				return true;
			} else {
				break _v14_2;
			}
		}
	} while(false);
	return false;
};
var _elm_lang$core$Dict$sizeHelp = F2(
	function (n, dict) {
		sizeHelp:
		while (true) {
			var _p14 = dict;
			if (_p14.ctor === 'RBEmpty_elm_builtin') {
				return n;
			} else {
				var _v16 = A2(_elm_lang$core$Dict$sizeHelp, n + 1, _p14._4),
					_v17 = _p14._3;
				n = _v16;
				dict = _v17;
				continue sizeHelp;
			}
		}
	});
var _elm_lang$core$Dict$size = function (dict) {
	return A2(_elm_lang$core$Dict$sizeHelp, 0, dict);
};
var _elm_lang$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			var _p15 = dict;
			if (_p15.ctor === 'RBEmpty_elm_builtin') {
				return _elm_lang$core$Maybe$Nothing;
			} else {
				var _p16 = A2(_elm_lang$core$Basics$compare, targetKey, _p15._1);
				switch (_p16.ctor) {
					case 'LT':
						var _v20 = targetKey,
							_v21 = _p15._3;
						targetKey = _v20;
						dict = _v21;
						continue get;
					case 'EQ':
						return _elm_lang$core$Maybe$Just(_p15._2);
					default:
						var _v22 = targetKey,
							_v23 = _p15._4;
						targetKey = _v22;
						dict = _v23;
						continue get;
				}
			}
		}
	});
var _elm_lang$core$Dict$member = F2(
	function (key, dict) {
		var _p17 = A2(_elm_lang$core$Dict$get, key, dict);
		if (_p17.ctor === 'Just') {
			return true;
		} else {
			return false;
		}
	});
var _elm_lang$core$Dict$maxWithDefault = F3(
	function (k, v, r) {
		maxWithDefault:
		while (true) {
			var _p18 = r;
			if (_p18.ctor === 'RBEmpty_elm_builtin') {
				return {ctor: '_Tuple2', _0: k, _1: v};
			} else {
				var _v26 = _p18._1,
					_v27 = _p18._2,
					_v28 = _p18._4;
				k = _v26;
				v = _v27;
				r = _v28;
				continue maxWithDefault;
			}
		}
	});
var _elm_lang$core$Dict$NBlack = {ctor: 'NBlack'};
var _elm_lang$core$Dict$BBlack = {ctor: 'BBlack'};
var _elm_lang$core$Dict$Black = {ctor: 'Black'};
var _elm_lang$core$Dict$blackish = function (t) {
	var _p19 = t;
	if (_p19.ctor === 'RBNode_elm_builtin') {
		var _p20 = _p19._0;
		return _elm_lang$core$Native_Utils.eq(_p20, _elm_lang$core$Dict$Black) || _elm_lang$core$Native_Utils.eq(_p20, _elm_lang$core$Dict$BBlack);
	} else {
		return true;
	}
};
var _elm_lang$core$Dict$Red = {ctor: 'Red'};
var _elm_lang$core$Dict$moreBlack = function (color) {
	var _p21 = color;
	switch (_p21.ctor) {
		case 'Black':
			return _elm_lang$core$Dict$BBlack;
		case 'Red':
			return _elm_lang$core$Dict$Black;
		case 'NBlack':
			return _elm_lang$core$Dict$Red;
		default:
			return _elm_lang$core$Native_Debug.crash('Can\'t make a double black node more black!');
	}
};
var _elm_lang$core$Dict$lessBlack = function (color) {
	var _p22 = color;
	switch (_p22.ctor) {
		case 'BBlack':
			return _elm_lang$core$Dict$Black;
		case 'Black':
			return _elm_lang$core$Dict$Red;
		case 'Red':
			return _elm_lang$core$Dict$NBlack;
		default:
			return _elm_lang$core$Native_Debug.crash('Can\'t make a negative black node less black!');
	}
};
var _elm_lang$core$Dict$LBBlack = {ctor: 'LBBlack'};
var _elm_lang$core$Dict$LBlack = {ctor: 'LBlack'};
var _elm_lang$core$Dict$RBEmpty_elm_builtin = function (a) {
	return {ctor: 'RBEmpty_elm_builtin', _0: a};
};
var _elm_lang$core$Dict$empty = _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
var _elm_lang$core$Dict$isEmpty = function (dict) {
	return _elm_lang$core$Native_Utils.eq(dict, _elm_lang$core$Dict$empty);
};
var _elm_lang$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {ctor: 'RBNode_elm_builtin', _0: a, _1: b, _2: c, _3: d, _4: e};
	});
var _elm_lang$core$Dict$ensureBlackRoot = function (dict) {
	var _p23 = dict;
	if ((_p23.ctor === 'RBNode_elm_builtin') && (_p23._0.ctor === 'Red')) {
		return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p23._1, _p23._2, _p23._3, _p23._4);
	} else {
		return dict;
	}
};
var _elm_lang$core$Dict$lessBlackTree = function (dict) {
	var _p24 = dict;
	if (_p24.ctor === 'RBNode_elm_builtin') {
		return A5(
			_elm_lang$core$Dict$RBNode_elm_builtin,
			_elm_lang$core$Dict$lessBlack(_p24._0),
			_p24._1,
			_p24._2,
			_p24._3,
			_p24._4);
	} else {
		return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
	}
};
var _elm_lang$core$Dict$balancedTree = function (col) {
	return function (xk) {
		return function (xv) {
			return function (yk) {
				return function (yv) {
					return function (zk) {
						return function (zv) {
							return function (a) {
								return function (b) {
									return function (c) {
										return function (d) {
											return A5(
												_elm_lang$core$Dict$RBNode_elm_builtin,
												_elm_lang$core$Dict$lessBlack(col),
												yk,
												yv,
												A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, xk, xv, a, b),
												A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, zk, zv, c, d));
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var _elm_lang$core$Dict$blacken = function (t) {
	var _p25 = t;
	if (_p25.ctor === 'RBEmpty_elm_builtin') {
		return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
	} else {
		return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p25._1, _p25._2, _p25._3, _p25._4);
	}
};
var _elm_lang$core$Dict$redden = function (t) {
	var _p26 = t;
	if (_p26.ctor === 'RBEmpty_elm_builtin') {
		return _elm_lang$core$Native_Debug.crash('can\'t make a Leaf red');
	} else {
		return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Red, _p26._1, _p26._2, _p26._3, _p26._4);
	}
};
var _elm_lang$core$Dict$balanceHelp = function (tree) {
	var _p27 = tree;
	_v36_6:
	do {
		_v36_5:
		do {
			_v36_4:
			do {
				_v36_3:
				do {
					_v36_2:
					do {
						_v36_1:
						do {
							_v36_0:
							do {
								if (_p27.ctor === 'RBNode_elm_builtin') {
									if (_p27._3.ctor === 'RBNode_elm_builtin') {
										if (_p27._4.ctor === 'RBNode_elm_builtin') {
											switch (_p27._3._0.ctor) {
												case 'Red':
													switch (_p27._4._0.ctor) {
														case 'Red':
															if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
																break _v36_0;
															} else {
																if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
																	break _v36_1;
																} else {
																	if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
																		break _v36_2;
																	} else {
																		if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
																			break _v36_3;
																		} else {
																			break _v36_6;
																		}
																	}
																}
															}
														case 'NBlack':
															if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
																break _v36_0;
															} else {
																if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
																	break _v36_1;
																} else {
																	if (((((_p27._0.ctor === 'BBlack') && (_p27._4._3.ctor === 'RBNode_elm_builtin')) && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
																		break _v36_4;
																	} else {
																		break _v36_6;
																	}
																}
															}
														default:
															if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
																break _v36_0;
															} else {
																if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
																	break _v36_1;
																} else {
																	break _v36_6;
																}
															}
													}
												case 'NBlack':
													switch (_p27._4._0.ctor) {
														case 'Red':
															if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
																break _v36_2;
															} else {
																if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
																	break _v36_3;
																} else {
																	if (((((_p27._0.ctor === 'BBlack') && (_p27._3._3.ctor === 'RBNode_elm_builtin')) && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
																		break _v36_5;
																	} else {
																		break _v36_6;
																	}
																}
															}
														case 'NBlack':
															if (_p27._0.ctor === 'BBlack') {
																if ((((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
																	break _v36_4;
																} else {
																	if ((((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
																		break _v36_5;
																	} else {
																		break _v36_6;
																	}
																}
															} else {
																break _v36_6;
															}
														default:
															if (((((_p27._0.ctor === 'BBlack') && (_p27._3._3.ctor === 'RBNode_elm_builtin')) && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
																break _v36_5;
															} else {
																break _v36_6;
															}
													}
												default:
													switch (_p27._4._0.ctor) {
														case 'Red':
															if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
																break _v36_2;
															} else {
																if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
																	break _v36_3;
																} else {
																	break _v36_6;
																}
															}
														case 'NBlack':
															if (((((_p27._0.ctor === 'BBlack') && (_p27._4._3.ctor === 'RBNode_elm_builtin')) && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
																break _v36_4;
															} else {
																break _v36_6;
															}
														default:
															break _v36_6;
													}
											}
										} else {
											switch (_p27._3._0.ctor) {
												case 'Red':
													if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
														break _v36_0;
													} else {
														if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
															break _v36_1;
														} else {
															break _v36_6;
														}
													}
												case 'NBlack':
													if (((((_p27._0.ctor === 'BBlack') && (_p27._3._3.ctor === 'RBNode_elm_builtin')) && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
														break _v36_5;
													} else {
														break _v36_6;
													}
												default:
													break _v36_6;
											}
										}
									} else {
										if (_p27._4.ctor === 'RBNode_elm_builtin') {
											switch (_p27._4._0.ctor) {
												case 'Red':
													if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
														break _v36_2;
													} else {
														if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
															break _v36_3;
														} else {
															break _v36_6;
														}
													}
												case 'NBlack':
													if (((((_p27._0.ctor === 'BBlack') && (_p27._4._3.ctor === 'RBNode_elm_builtin')) && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
														break _v36_4;
													} else {
														break _v36_6;
													}
												default:
													break _v36_6;
											}
										} else {
											break _v36_6;
										}
									}
								} else {
									break _v36_6;
								}
							} while(false);
							return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._3._3._1)(_p27._3._3._2)(_p27._3._1)(_p27._3._2)(_p27._1)(_p27._2)(_p27._3._3._3)(_p27._3._3._4)(_p27._3._4)(_p27._4);
						} while(false);
						return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._3._1)(_p27._3._2)(_p27._3._4._1)(_p27._3._4._2)(_p27._1)(_p27._2)(_p27._3._3)(_p27._3._4._3)(_p27._3._4._4)(_p27._4);
					} while(false);
					return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._1)(_p27._2)(_p27._4._3._1)(_p27._4._3._2)(_p27._4._1)(_p27._4._2)(_p27._3)(_p27._4._3._3)(_p27._4._3._4)(_p27._4._4);
				} while(false);
				return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._1)(_p27._2)(_p27._4._1)(_p27._4._2)(_p27._4._4._1)(_p27._4._4._2)(_p27._3)(_p27._4._3)(_p27._4._4._3)(_p27._4._4._4);
			} while(false);
			return A5(
				_elm_lang$core$Dict$RBNode_elm_builtin,
				_elm_lang$core$Dict$Black,
				_p27._4._3._1,
				_p27._4._3._2,
				A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p27._1, _p27._2, _p27._3, _p27._4._3._3),
				A5(
					_elm_lang$core$Dict$balance,
					_elm_lang$core$Dict$Black,
					_p27._4._1,
					_p27._4._2,
					_p27._4._3._4,
					_elm_lang$core$Dict$redden(_p27._4._4)));
		} while(false);
		return A5(
			_elm_lang$core$Dict$RBNode_elm_builtin,
			_elm_lang$core$Dict$Black,
			_p27._3._4._1,
			_p27._3._4._2,
			A5(
				_elm_lang$core$Dict$balance,
				_elm_lang$core$Dict$Black,
				_p27._3._1,
				_p27._3._2,
				_elm_lang$core$Dict$redden(_p27._3._3),
				_p27._3._4._3),
			A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p27._1, _p27._2, _p27._3._4._4, _p27._4));
	} while(false);
	return tree;
};
var _elm_lang$core$Dict$balance = F5(
	function (c, k, v, l, r) {
		var tree = A5(_elm_lang$core$Dict$RBNode_elm_builtin, c, k, v, l, r);
		return _elm_lang$core$Dict$blackish(tree) ? _elm_lang$core$Dict$balanceHelp(tree) : tree;
	});
var _elm_lang$core$Dict$bubble = F5(
	function (c, k, v, l, r) {
		return (_elm_lang$core$Dict$isBBlack(l) || _elm_lang$core$Dict$isBBlack(r)) ? A5(
			_elm_lang$core$Dict$balance,
			_elm_lang$core$Dict$moreBlack(c),
			k,
			v,
			_elm_lang$core$Dict$lessBlackTree(l),
			_elm_lang$core$Dict$lessBlackTree(r)) : A5(_elm_lang$core$Dict$RBNode_elm_builtin, c, k, v, l, r);
	});
var _elm_lang$core$Dict$removeMax = F5(
	function (c, k, v, l, r) {
		var _p28 = r;
		if (_p28.ctor === 'RBEmpty_elm_builtin') {
			return A3(_elm_lang$core$Dict$rem, c, l, r);
		} else {
			return A5(
				_elm_lang$core$Dict$bubble,
				c,
				k,
				v,
				l,
				A5(_elm_lang$core$Dict$removeMax, _p28._0, _p28._1, _p28._2, _p28._3, _p28._4));
		}
	});
var _elm_lang$core$Dict$rem = F3(
	function (color, left, right) {
		var _p29 = {ctor: '_Tuple2', _0: left, _1: right};
		if (_p29._0.ctor === 'RBEmpty_elm_builtin') {
			if (_p29._1.ctor === 'RBEmpty_elm_builtin') {
				var _p30 = color;
				switch (_p30.ctor) {
					case 'Red':
						return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
					case 'Black':
						return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBBlack);
					default:
						return _elm_lang$core$Native_Debug.crash('cannot have bblack or nblack nodes at this point');
				}
			} else {
				var _p33 = _p29._1._0;
				var _p32 = _p29._0._0;
				var _p31 = {ctor: '_Tuple3', _0: color, _1: _p32, _2: _p33};
				if ((((_p31.ctor === '_Tuple3') && (_p31._0.ctor === 'Black')) && (_p31._1.ctor === 'LBlack')) && (_p31._2.ctor === 'Red')) {
					return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p29._1._1, _p29._1._2, _p29._1._3, _p29._1._4);
				} else {
					return A4(
						_elm_lang$core$Dict$reportRemBug,
						'Black/LBlack/Red',
						color,
						_elm_lang$core$Basics$toString(_p32),
						_elm_lang$core$Basics$toString(_p33));
				}
			}
		} else {
			if (_p29._1.ctor === 'RBEmpty_elm_builtin') {
				var _p36 = _p29._1._0;
				var _p35 = _p29._0._0;
				var _p34 = {ctor: '_Tuple3', _0: color, _1: _p35, _2: _p36};
				if ((((_p34.ctor === '_Tuple3') && (_p34._0.ctor === 'Black')) && (_p34._1.ctor === 'Red')) && (_p34._2.ctor === 'LBlack')) {
					return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p29._0._1, _p29._0._2, _p29._0._3, _p29._0._4);
				} else {
					return A4(
						_elm_lang$core$Dict$reportRemBug,
						'Black/Red/LBlack',
						color,
						_elm_lang$core$Basics$toString(_p35),
						_elm_lang$core$Basics$toString(_p36));
				}
			} else {
				var _p40 = _p29._0._2;
				var _p39 = _p29._0._4;
				var _p38 = _p29._0._1;
				var newLeft = A5(_elm_lang$core$Dict$removeMax, _p29._0._0, _p38, _p40, _p29._0._3, _p39);
				var _p37 = A3(_elm_lang$core$Dict$maxWithDefault, _p38, _p40, _p39);
				var k = _p37._0;
				var v = _p37._1;
				return A5(_elm_lang$core$Dict$bubble, color, k, v, newLeft, right);
			}
		}
	});
var _elm_lang$core$Dict$map = F2(
	function (f, dict) {
		var _p41 = dict;
		if (_p41.ctor === 'RBEmpty_elm_builtin') {
			return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
		} else {
			var _p42 = _p41._1;
			return A5(
				_elm_lang$core$Dict$RBNode_elm_builtin,
				_p41._0,
				_p42,
				A2(f, _p42, _p41._2),
				A2(_elm_lang$core$Dict$map, f, _p41._3),
				A2(_elm_lang$core$Dict$map, f, _p41._4));
		}
	});
var _elm_lang$core$Dict$Same = {ctor: 'Same'};
var _elm_lang$core$Dict$Remove = {ctor: 'Remove'};
var _elm_lang$core$Dict$Insert = {ctor: 'Insert'};
var _elm_lang$core$Dict$update = F3(
	function (k, alter, dict) {
		var up = function (dict) {
			var _p43 = dict;
			if (_p43.ctor === 'RBEmpty_elm_builtin') {
				var _p44 = alter(_elm_lang$core$Maybe$Nothing);
				if (_p44.ctor === 'Nothing') {
					return {ctor: '_Tuple2', _0: _elm_lang$core$Dict$Same, _1: _elm_lang$core$Dict$empty};
				} else {
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Dict$Insert,
						_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Red, k, _p44._0, _elm_lang$core$Dict$empty, _elm_lang$core$Dict$empty)
					};
				}
			} else {
				var _p55 = _p43._2;
				var _p54 = _p43._4;
				var _p53 = _p43._3;
				var _p52 = _p43._1;
				var _p51 = _p43._0;
				var _p45 = A2(_elm_lang$core$Basics$compare, k, _p52);
				switch (_p45.ctor) {
					case 'EQ':
						var _p46 = alter(
							_elm_lang$core$Maybe$Just(_p55));
						if (_p46.ctor === 'Nothing') {
							return {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Dict$Remove,
								_1: A3(_elm_lang$core$Dict$rem, _p51, _p53, _p54)
							};
						} else {
							return {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Dict$Same,
								_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _p51, _p52, _p46._0, _p53, _p54)
							};
						}
					case 'LT':
						var _p47 = up(_p53);
						var flag = _p47._0;
						var newLeft = _p47._1;
						var _p48 = flag;
						switch (_p48.ctor) {
							case 'Same':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Same,
									_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _p51, _p52, _p55, newLeft, _p54)
								};
							case 'Insert':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Insert,
									_1: A5(_elm_lang$core$Dict$balance, _p51, _p52, _p55, newLeft, _p54)
								};
							default:
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Remove,
									_1: A5(_elm_lang$core$Dict$bubble, _p51, _p52, _p55, newLeft, _p54)
								};
						}
					default:
						var _p49 = up(_p54);
						var flag = _p49._0;
						var newRight = _p49._1;
						var _p50 = flag;
						switch (_p50.ctor) {
							case 'Same':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Same,
									_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _p51, _p52, _p55, _p53, newRight)
								};
							case 'Insert':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Insert,
									_1: A5(_elm_lang$core$Dict$balance, _p51, _p52, _p55, _p53, newRight)
								};
							default:
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Remove,
									_1: A5(_elm_lang$core$Dict$bubble, _p51, _p52, _p55, _p53, newRight)
								};
						}
				}
			}
		};
		var _p56 = up(dict);
		var flag = _p56._0;
		var updatedDict = _p56._1;
		var _p57 = flag;
		switch (_p57.ctor) {
			case 'Same':
				return updatedDict;
			case 'Insert':
				return _elm_lang$core$Dict$ensureBlackRoot(updatedDict);
			default:
				return _elm_lang$core$Dict$blacken(updatedDict);
		}
	});
var _elm_lang$core$Dict$insert = F3(
	function (key, value, dict) {
		return A3(
			_elm_lang$core$Dict$update,
			key,
			_elm_lang$core$Basics$always(
				_elm_lang$core$Maybe$Just(value)),
			dict);
	});
var _elm_lang$core$Dict$singleton = F2(
	function (key, value) {
		return A3(_elm_lang$core$Dict$insert, key, value, _elm_lang$core$Dict$empty);
	});
var _elm_lang$core$Dict$union = F2(
	function (t1, t2) {
		return A3(_elm_lang$core$Dict$foldl, _elm_lang$core$Dict$insert, t2, t1);
	});
var _elm_lang$core$Dict$filter = F2(
	function (predicate, dictionary) {
		var add = F3(
			function (key, value, dict) {
				return A2(predicate, key, value) ? A3(_elm_lang$core$Dict$insert, key, value, dict) : dict;
			});
		return A3(_elm_lang$core$Dict$foldl, add, _elm_lang$core$Dict$empty, dictionary);
	});
var _elm_lang$core$Dict$intersect = F2(
	function (t1, t2) {
		return A2(
			_elm_lang$core$Dict$filter,
			F2(
				function (k, _p58) {
					return A2(_elm_lang$core$Dict$member, k, t2);
				}),
			t1);
	});
var _elm_lang$core$Dict$partition = F2(
	function (predicate, dict) {
		var add = F3(
			function (key, value, _p59) {
				var _p60 = _p59;
				var _p62 = _p60._1;
				var _p61 = _p60._0;
				return A2(predicate, key, value) ? {
					ctor: '_Tuple2',
					_0: A3(_elm_lang$core$Dict$insert, key, value, _p61),
					_1: _p62
				} : {
					ctor: '_Tuple2',
					_0: _p61,
					_1: A3(_elm_lang$core$Dict$insert, key, value, _p62)
				};
			});
		return A3(
			_elm_lang$core$Dict$foldl,
			add,
			{ctor: '_Tuple2', _0: _elm_lang$core$Dict$empty, _1: _elm_lang$core$Dict$empty},
			dict);
	});
var _elm_lang$core$Dict$fromList = function (assocs) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (_p63, dict) {
				var _p64 = _p63;
				return A3(_elm_lang$core$Dict$insert, _p64._0, _p64._1, dict);
			}),
		_elm_lang$core$Dict$empty,
		assocs);
};
var _elm_lang$core$Dict$remove = F2(
	function (key, dict) {
		return A3(
			_elm_lang$core$Dict$update,
			key,
			_elm_lang$core$Basics$always(_elm_lang$core$Maybe$Nothing),
			dict);
	});
var _elm_lang$core$Dict$diff = F2(
	function (t1, t2) {
		return A3(
			_elm_lang$core$Dict$foldl,
			F3(
				function (k, v, t) {
					return A2(_elm_lang$core$Dict$remove, k, t);
				}),
			t1,
			t2);
	});

//import Native.Scheduler //

var _elm_lang$core$Native_Time = function() {

var now = _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
{
	callback(_elm_lang$core$Native_Scheduler.succeed(Date.now()));
});

function setInterval_(interval, task)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		var id = setInterval(function() {
			_elm_lang$core$Native_Scheduler.rawSpawn(task);
		}, interval);

		return function() { clearInterval(id); };
	});
}

return {
	now: now,
	setInterval_: F2(setInterval_)
};

}();
var _elm_lang$core$Task$onError = _elm_lang$core$Native_Scheduler.onError;
var _elm_lang$core$Task$andThen = _elm_lang$core$Native_Scheduler.andThen;
var _elm_lang$core$Task$spawnCmd = F2(
	function (router, _p0) {
		var _p1 = _p0;
		return _elm_lang$core$Native_Scheduler.spawn(
			A2(
				_elm_lang$core$Task$andThen,
				_elm_lang$core$Platform$sendToApp(router),
				_p1._0));
	});
var _elm_lang$core$Task$fail = _elm_lang$core$Native_Scheduler.fail;
var _elm_lang$core$Task$mapError = F2(
	function (convert, task) {
		return A2(
			_elm_lang$core$Task$onError,
			function (_p2) {
				return _elm_lang$core$Task$fail(
					convert(_p2));
			},
			task);
	});
var _elm_lang$core$Task$succeed = _elm_lang$core$Native_Scheduler.succeed;
var _elm_lang$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return _elm_lang$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var _elm_lang$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (b) {
						return _elm_lang$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var _elm_lang$core$Task$map3 = F4(
	function (func, taskA, taskB, taskC) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (b) {
						return A2(
							_elm_lang$core$Task$andThen,
							function (c) {
								return _elm_lang$core$Task$succeed(
									A3(func, a, b, c));
							},
							taskC);
					},
					taskB);
			},
			taskA);
	});
var _elm_lang$core$Task$map4 = F5(
	function (func, taskA, taskB, taskC, taskD) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (b) {
						return A2(
							_elm_lang$core$Task$andThen,
							function (c) {
								return A2(
									_elm_lang$core$Task$andThen,
									function (d) {
										return _elm_lang$core$Task$succeed(
											A4(func, a, b, c, d));
									},
									taskD);
							},
							taskC);
					},
					taskB);
			},
			taskA);
	});
var _elm_lang$core$Task$map5 = F6(
	function (func, taskA, taskB, taskC, taskD, taskE) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (b) {
						return A2(
							_elm_lang$core$Task$andThen,
							function (c) {
								return A2(
									_elm_lang$core$Task$andThen,
									function (d) {
										return A2(
											_elm_lang$core$Task$andThen,
											function (e) {
												return _elm_lang$core$Task$succeed(
													A5(func, a, b, c, d, e));
											},
											taskE);
									},
									taskD);
							},
							taskC);
					},
					taskB);
			},
			taskA);
	});
var _elm_lang$core$Task$sequence = function (tasks) {
	var _p3 = tasks;
	if (_p3.ctor === '[]') {
		return _elm_lang$core$Task$succeed(
			{ctor: '[]'});
	} else {
		return A3(
			_elm_lang$core$Task$map2,
			F2(
				function (x, y) {
					return {ctor: '::', _0: x, _1: y};
				}),
			_p3._0,
			_elm_lang$core$Task$sequence(_p3._1));
	}
};
var _elm_lang$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			_elm_lang$core$Task$map,
			function (_p4) {
				return {ctor: '_Tuple0'};
			},
			_elm_lang$core$Task$sequence(
				A2(
					_elm_lang$core$List$map,
					_elm_lang$core$Task$spawnCmd(router),
					commands)));
	});
var _elm_lang$core$Task$init = _elm_lang$core$Task$succeed(
	{ctor: '_Tuple0'});
var _elm_lang$core$Task$onSelfMsg = F3(
	function (_p7, _p6, _p5) {
		return _elm_lang$core$Task$succeed(
			{ctor: '_Tuple0'});
	});
var _elm_lang$core$Task$command = _elm_lang$core$Native_Platform.leaf('Task');
var _elm_lang$core$Task$Perform = function (a) {
	return {ctor: 'Perform', _0: a};
};
var _elm_lang$core$Task$perform = F2(
	function (toMessage, task) {
		return _elm_lang$core$Task$command(
			_elm_lang$core$Task$Perform(
				A2(_elm_lang$core$Task$map, toMessage, task)));
	});
var _elm_lang$core$Task$attempt = F2(
	function (resultToMessage, task) {
		return _elm_lang$core$Task$command(
			_elm_lang$core$Task$Perform(
				A2(
					_elm_lang$core$Task$onError,
					function (_p8) {
						return _elm_lang$core$Task$succeed(
							resultToMessage(
								_elm_lang$core$Result$Err(_p8)));
					},
					A2(
						_elm_lang$core$Task$andThen,
						function (_p9) {
							return _elm_lang$core$Task$succeed(
								resultToMessage(
									_elm_lang$core$Result$Ok(_p9)));
						},
						task))));
	});
var _elm_lang$core$Task$cmdMap = F2(
	function (tagger, _p10) {
		var _p11 = _p10;
		return _elm_lang$core$Task$Perform(
			A2(_elm_lang$core$Task$map, tagger, _p11._0));
	});
_elm_lang$core$Native_Platform.effectManagers['Task'] = {pkg: 'elm-lang/core', init: _elm_lang$core$Task$init, onEffects: _elm_lang$core$Task$onEffects, onSelfMsg: _elm_lang$core$Task$onSelfMsg, tag: 'cmd', cmdMap: _elm_lang$core$Task$cmdMap};

var _elm_lang$core$Time$setInterval = _elm_lang$core$Native_Time.setInterval_;
var _elm_lang$core$Time$spawnHelp = F3(
	function (router, intervals, processes) {
		var _p0 = intervals;
		if (_p0.ctor === '[]') {
			return _elm_lang$core$Task$succeed(processes);
		} else {
			var _p1 = _p0._0;
			var spawnRest = function (id) {
				return A3(
					_elm_lang$core$Time$spawnHelp,
					router,
					_p0._1,
					A3(_elm_lang$core$Dict$insert, _p1, id, processes));
			};
			var spawnTimer = _elm_lang$core$Native_Scheduler.spawn(
				A2(
					_elm_lang$core$Time$setInterval,
					_p1,
					A2(_elm_lang$core$Platform$sendToSelf, router, _p1)));
			return A2(_elm_lang$core$Task$andThen, spawnRest, spawnTimer);
		}
	});
var _elm_lang$core$Time$addMySub = F2(
	function (_p2, state) {
		var _p3 = _p2;
		var _p6 = _p3._1;
		var _p5 = _p3._0;
		var _p4 = A2(_elm_lang$core$Dict$get, _p5, state);
		if (_p4.ctor === 'Nothing') {
			return A3(
				_elm_lang$core$Dict$insert,
				_p5,
				{
					ctor: '::',
					_0: _p6,
					_1: {ctor: '[]'}
				},
				state);
		} else {
			return A3(
				_elm_lang$core$Dict$insert,
				_p5,
				{ctor: '::', _0: _p6, _1: _p4._0},
				state);
		}
	});
var _elm_lang$core$Time$inMilliseconds = function (t) {
	return t;
};
var _elm_lang$core$Time$millisecond = 1;
var _elm_lang$core$Time$second = 1000 * _elm_lang$core$Time$millisecond;
var _elm_lang$core$Time$minute = 60 * _elm_lang$core$Time$second;
var _elm_lang$core$Time$hour = 60 * _elm_lang$core$Time$minute;
var _elm_lang$core$Time$inHours = function (t) {
	return t / _elm_lang$core$Time$hour;
};
var _elm_lang$core$Time$inMinutes = function (t) {
	return t / _elm_lang$core$Time$minute;
};
var _elm_lang$core$Time$inSeconds = function (t) {
	return t / _elm_lang$core$Time$second;
};
var _elm_lang$core$Time$now = _elm_lang$core$Native_Time.now;
var _elm_lang$core$Time$onSelfMsg = F3(
	function (router, interval, state) {
		var _p7 = A2(_elm_lang$core$Dict$get, interval, state.taggers);
		if (_p7.ctor === 'Nothing') {
			return _elm_lang$core$Task$succeed(state);
		} else {
			var tellTaggers = function (time) {
				return _elm_lang$core$Task$sequence(
					A2(
						_elm_lang$core$List$map,
						function (tagger) {
							return A2(
								_elm_lang$core$Platform$sendToApp,
								router,
								tagger(time));
						},
						_p7._0));
			};
			return A2(
				_elm_lang$core$Task$andThen,
				function (_p8) {
					return _elm_lang$core$Task$succeed(state);
				},
				A2(_elm_lang$core$Task$andThen, tellTaggers, _elm_lang$core$Time$now));
		}
	});
var _elm_lang$core$Time$subscription = _elm_lang$core$Native_Platform.leaf('Time');
var _elm_lang$core$Time$State = F2(
	function (a, b) {
		return {taggers: a, processes: b};
	});
var _elm_lang$core$Time$init = _elm_lang$core$Task$succeed(
	A2(_elm_lang$core$Time$State, _elm_lang$core$Dict$empty, _elm_lang$core$Dict$empty));
var _elm_lang$core$Time$onEffects = F3(
	function (router, subs, _p9) {
		var _p10 = _p9;
		var rightStep = F3(
			function (_p12, id, _p11) {
				var _p13 = _p11;
				return {
					ctor: '_Tuple3',
					_0: _p13._0,
					_1: _p13._1,
					_2: A2(
						_elm_lang$core$Task$andThen,
						function (_p14) {
							return _p13._2;
						},
						_elm_lang$core$Native_Scheduler.kill(id))
				};
			});
		var bothStep = F4(
			function (interval, taggers, id, _p15) {
				var _p16 = _p15;
				return {
					ctor: '_Tuple3',
					_0: _p16._0,
					_1: A3(_elm_lang$core$Dict$insert, interval, id, _p16._1),
					_2: _p16._2
				};
			});
		var leftStep = F3(
			function (interval, taggers, _p17) {
				var _p18 = _p17;
				return {
					ctor: '_Tuple3',
					_0: {ctor: '::', _0: interval, _1: _p18._0},
					_1: _p18._1,
					_2: _p18._2
				};
			});
		var newTaggers = A3(_elm_lang$core$List$foldl, _elm_lang$core$Time$addMySub, _elm_lang$core$Dict$empty, subs);
		var _p19 = A6(
			_elm_lang$core$Dict$merge,
			leftStep,
			bothStep,
			rightStep,
			newTaggers,
			_p10.processes,
			{
				ctor: '_Tuple3',
				_0: {ctor: '[]'},
				_1: _elm_lang$core$Dict$empty,
				_2: _elm_lang$core$Task$succeed(
					{ctor: '_Tuple0'})
			});
		var spawnList = _p19._0;
		var existingDict = _p19._1;
		var killTask = _p19._2;
		return A2(
			_elm_lang$core$Task$andThen,
			function (newProcesses) {
				return _elm_lang$core$Task$succeed(
					A2(_elm_lang$core$Time$State, newTaggers, newProcesses));
			},
			A2(
				_elm_lang$core$Task$andThen,
				function (_p20) {
					return A3(_elm_lang$core$Time$spawnHelp, router, spawnList, existingDict);
				},
				killTask));
	});
var _elm_lang$core$Time$Every = F2(
	function (a, b) {
		return {ctor: 'Every', _0: a, _1: b};
	});
var _elm_lang$core$Time$every = F2(
	function (interval, tagger) {
		return _elm_lang$core$Time$subscription(
			A2(_elm_lang$core$Time$Every, interval, tagger));
	});
var _elm_lang$core$Time$subMap = F2(
	function (f, _p21) {
		var _p22 = _p21;
		return A2(
			_elm_lang$core$Time$Every,
			_p22._0,
			function (_p23) {
				return f(
					_p22._1(_p23));
			});
	});
_elm_lang$core$Native_Platform.effectManagers['Time'] = {pkg: 'elm-lang/core', init: _elm_lang$core$Time$init, onEffects: _elm_lang$core$Time$onEffects, onSelfMsg: _elm_lang$core$Time$onSelfMsg, tag: 'sub', subMap: _elm_lang$core$Time$subMap};

var _Bogdanp$elm_time$Time_DateTime$isValidTime = F4(
	function (hour, minute, second, millisecond) {
		return (_elm_lang$core$Native_Utils.cmp(hour, 0) > -1) && ((_elm_lang$core$Native_Utils.cmp(hour, 24) < 0) && ((_elm_lang$core$Native_Utils.cmp(minute, 0) > -1) && ((_elm_lang$core$Native_Utils.cmp(minute, 60) < 0) && ((_elm_lang$core$Native_Utils.cmp(second, 0) > -1) && ((_elm_lang$core$Native_Utils.cmp(second, 60) < 0) && ((_elm_lang$core$Native_Utils.cmp(millisecond, 0) > -1) && (_elm_lang$core$Native_Utils.cmp(millisecond, 1000) < 0)))))));
	});
var _Bogdanp$elm_time$Time_DateTime$delta = F2(
	function (_p1, _p0) {
		var _p2 = _p1;
		var _p6 = _p2._0;
		var _p3 = _p0;
		var _p5 = _p3._0;
		var _p4 = A2(_Bogdanp$elm_time$Time_Date$delta, _p6.date, _p5.date);
		var years = _p4.years;
		var months = _p4.months;
		var days = _p4.days;
		var milliseconds = (days * _Bogdanp$elm_time$Time_Internal$dayMs) + (_p6.offset - _p5.offset);
		var hours = (milliseconds / _Bogdanp$elm_time$Time_Internal$hourMs) | 0;
		var minutes = (milliseconds / _Bogdanp$elm_time$Time_Internal$minuteMs) | 0;
		var seconds = (milliseconds / _Bogdanp$elm_time$Time_Internal$secondMs) | 0;
		return {years: years, months: months, days: days, hours: hours, minutes: minutes, seconds: seconds, milliseconds: milliseconds};
	});
var _Bogdanp$elm_time$Time_DateTime$millisecond = function (_p7) {
	var _p8 = _p7;
	return A2(
		_elm_lang$core$Basics_ops['%'],
		A2(
			_elm_lang$core$Basics_ops['%'],
			A2(_elm_lang$core$Basics_ops['%'], _p8._0.offset, _Bogdanp$elm_time$Time_Internal$hourMs),
			_Bogdanp$elm_time$Time_Internal$minuteMs),
		_Bogdanp$elm_time$Time_Internal$secondMs);
};
var _Bogdanp$elm_time$Time_DateTime$second = function (_p9) {
	var _p10 = _p9;
	return (A2(
		_elm_lang$core$Basics_ops['%'],
		A2(_elm_lang$core$Basics_ops['%'], _p10._0.offset, _Bogdanp$elm_time$Time_Internal$hourMs),
		_Bogdanp$elm_time$Time_Internal$minuteMs) / _Bogdanp$elm_time$Time_Internal$secondMs) | 0;
};
var _Bogdanp$elm_time$Time_DateTime$minute = function (_p11) {
	var _p12 = _p11;
	return (A2(_elm_lang$core$Basics_ops['%'], _p12._0.offset, _Bogdanp$elm_time$Time_Internal$hourMs) / _Bogdanp$elm_time$Time_Internal$minuteMs) | 0;
};
var _Bogdanp$elm_time$Time_DateTime$hour = function (_p13) {
	var _p14 = _p13;
	return (_p14._0.offset / _Bogdanp$elm_time$Time_Internal$hourMs) | 0;
};
var _Bogdanp$elm_time$Time_DateTime$toTuple = function (_p15) {
	var _p16 = _p15;
	var _p18 = _p16;
	var _p17 = _Bogdanp$elm_time$Time_Date$toTuple(_p16._0.date);
	var year = _p17._0;
	var month = _p17._1;
	var day = _p17._2;
	return {
		ctor: '_Tuple7',
		_0: year,
		_1: month,
		_2: day,
		_3: _Bogdanp$elm_time$Time_DateTime$hour(_p18),
		_4: _Bogdanp$elm_time$Time_DateTime$minute(_p18),
		_5: _Bogdanp$elm_time$Time_DateTime$second(_p18),
		_6: _Bogdanp$elm_time$Time_DateTime$millisecond(_p18)
	};
};
var _Bogdanp$elm_time$Time_DateTime$weekday = function (_p19) {
	var _p20 = _p19;
	return _Bogdanp$elm_time$Time_Date$weekday(_p20._0.date);
};
var _Bogdanp$elm_time$Time_DateTime$day = function (_p21) {
	var _p22 = _p21;
	return _Bogdanp$elm_time$Time_Date$day(_p22._0.date);
};
var _Bogdanp$elm_time$Time_DateTime$month = function (_p23) {
	var _p24 = _p23;
	return _Bogdanp$elm_time$Time_Date$month(_p24._0.date);
};
var _Bogdanp$elm_time$Time_DateTime$year = function (_p25) {
	var _p26 = _p25;
	return _Bogdanp$elm_time$Time_Date$year(_p26._0.date);
};
var _Bogdanp$elm_time$Time_DateTime$toISO8601 = function (time) {
	return A2(
		_elm_lang$core$Basics_ops['++'],
		_elm_lang$core$Basics$toString(
			_Bogdanp$elm_time$Time_DateTime$year(time)),
		A2(
			_elm_lang$core$Basics_ops['++'],
			'-',
			A2(
				_elm_lang$core$Basics_ops['++'],
				_Bogdanp$elm_time$Time_Internal$padded(
					_Bogdanp$elm_time$Time_DateTime$month(time)),
				A2(
					_elm_lang$core$Basics_ops['++'],
					'-',
					A2(
						_elm_lang$core$Basics_ops['++'],
						_Bogdanp$elm_time$Time_Internal$padded(
							_Bogdanp$elm_time$Time_DateTime$day(time)),
						A2(
							_elm_lang$core$Basics_ops['++'],
							'T',
							A2(
								_elm_lang$core$Basics_ops['++'],
								_Bogdanp$elm_time$Time_Internal$padded(
									_Bogdanp$elm_time$Time_DateTime$hour(time)),
								A2(
									_elm_lang$core$Basics_ops['++'],
									':',
									A2(
										_elm_lang$core$Basics_ops['++'],
										_Bogdanp$elm_time$Time_Internal$padded(
											_Bogdanp$elm_time$Time_DateTime$minute(time)),
										A2(
											_elm_lang$core$Basics_ops['++'],
											':',
											A2(
												_elm_lang$core$Basics_ops['++'],
												_Bogdanp$elm_time$Time_Internal$padded(
													_Bogdanp$elm_time$Time_DateTime$second(time)),
												'Z')))))))))));
};
var _Bogdanp$elm_time$Time_DateTime$compare = F2(
	function (dt1, dt2) {
		return A2(
			_elm_lang$core$Basics$compare,
			_Bogdanp$elm_time$Time_DateTime$toISO8601(dt1),
			_Bogdanp$elm_time$Time_DateTime$toISO8601(dt2));
	});
var _Bogdanp$elm_time$Time_DateTime$date = function (_p27) {
	var _p28 = _p27;
	return _p28._0.date;
};
var _Bogdanp$elm_time$Time_DateTime$zero = _Bogdanp$elm_time$Time_Internal$zero;
var _Bogdanp$elm_time$Time_DateTime$DateTimeDelta = F7(
	function (a, b, c, d, e, f, g) {
		return {years: a, months: b, days: c, hours: d, minutes: e, seconds: f, milliseconds: g};
	});
var _Bogdanp$elm_time$Time_DateTime$DateTime = function (a) {
	return {ctor: 'DateTime', _0: a};
};
var _Bogdanp$elm_time$Time_DateTime$dateTime = function (_p29) {
	var _p30 = _p29;
	return _Bogdanp$elm_time$Time_DateTime$DateTime(
		{
			date: A3(_Bogdanp$elm_time$Time_Date$date, _p30.year, _p30.month, _p30.day),
			offset: _Bogdanp$elm_time$Time_Internal$offsetFromTimeData(_p30)
		});
};
var _Bogdanp$elm_time$Time_DateTime$epoch = _Bogdanp$elm_time$Time_DateTime$dateTime(
	_elm_lang$core$Native_Utils.update(
		_Bogdanp$elm_time$Time_DateTime$zero,
		{year: 1970}));
var _Bogdanp$elm_time$Time_DateTime$toTimestamp = function (time) {
	return _elm_lang$core$Basics$toFloat(
		function (_) {
			return _.milliseconds;
		}(
			A2(_Bogdanp$elm_time$Time_DateTime$delta, time, _Bogdanp$elm_time$Time_DateTime$epoch)));
};
var _Bogdanp$elm_time$Time_DateTime$fromTuple = function (_p31) {
	var _p32 = _p31;
	return _Bogdanp$elm_time$Time_DateTime$dateTime(
		{year: _p32._0, month: _p32._1, day: _p32._2, hour: _p32._3, minute: _p32._4, second: _p32._5, millisecond: _p32._6});
};
var _Bogdanp$elm_time$Time_DateTime$mkDateTime = F2(
	function (date, time) {
		return _Bogdanp$elm_time$Time_DateTime$DateTime(
			{
				date: date,
				offset: _Bogdanp$elm_time$Time_Internal$offsetFromTimeData(time)
			});
	});
var _Bogdanp$elm_time$Time_DateTime$setHour = F2(
	function (hour, _p33) {
		var _p34 = _p33;
		var _p35 = _p34;
		return A2(
			_Bogdanp$elm_time$Time_DateTime$mkDateTime,
			_p34._0.date,
			{
				hour: hour,
				minute: _Bogdanp$elm_time$Time_DateTime$minute(_p35),
				second: _Bogdanp$elm_time$Time_DateTime$second(_p35),
				millisecond: _Bogdanp$elm_time$Time_DateTime$millisecond(_p35)
			});
	});
var _Bogdanp$elm_time$Time_DateTime$setMinute = F2(
	function (minute, _p36) {
		var _p37 = _p36;
		var _p38 = _p37;
		return A2(
			_Bogdanp$elm_time$Time_DateTime$mkDateTime,
			_p37._0.date,
			{
				hour: _Bogdanp$elm_time$Time_DateTime$hour(_p38),
				minute: minute,
				second: _Bogdanp$elm_time$Time_DateTime$second(_p38),
				millisecond: _Bogdanp$elm_time$Time_DateTime$millisecond(_p38)
			});
	});
var _Bogdanp$elm_time$Time_DateTime$setSecond = F2(
	function (second, _p39) {
		var _p40 = _p39;
		var _p41 = _p40;
		return A2(
			_Bogdanp$elm_time$Time_DateTime$mkDateTime,
			_p40._0.date,
			{
				hour: _Bogdanp$elm_time$Time_DateTime$hour(_p41),
				minute: _Bogdanp$elm_time$Time_DateTime$minute(_p41),
				second: second,
				millisecond: _Bogdanp$elm_time$Time_DateTime$millisecond(_p41)
			});
	});
var _Bogdanp$elm_time$Time_DateTime$setMillisecond = F2(
	function (millisecond, _p42) {
		var _p43 = _p42;
		var _p44 = _p43;
		return A2(
			_Bogdanp$elm_time$Time_DateTime$mkDateTime,
			_p43._0.date,
			{
				hour: _Bogdanp$elm_time$Time_DateTime$hour(_p44),
				minute: _Bogdanp$elm_time$Time_DateTime$minute(_p44),
				second: _Bogdanp$elm_time$Time_DateTime$second(_p44),
				millisecond: millisecond
			});
	});
var _Bogdanp$elm_time$Time_DateTime$setDate = F2(
	function (date, _p45) {
		var _p46 = _p45;
		return _Bogdanp$elm_time$Time_DateTime$DateTime(
			{date: date, offset: _p46._0.offset});
	});
var _Bogdanp$elm_time$Time_DateTime$setYear = F2(
	function (year, _p47) {
		var _p48 = _p47;
		return _Bogdanp$elm_time$Time_DateTime$DateTime(
			{
				date: A2(_Bogdanp$elm_time$Time_Date$setYear, year, _p48._0.date),
				offset: _p48._0.offset
			});
	});
var _Bogdanp$elm_time$Time_DateTime$setMonth = F2(
	function (month, _p49) {
		var _p50 = _p49;
		return _Bogdanp$elm_time$Time_DateTime$DateTime(
			{
				date: A2(_Bogdanp$elm_time$Time_Date$setMonth, month, _p50._0.date),
				offset: _p50._0.offset
			});
	});
var _Bogdanp$elm_time$Time_DateTime$setDay = F2(
	function (day, _p51) {
		var _p52 = _p51;
		return _Bogdanp$elm_time$Time_DateTime$DateTime(
			{
				date: A2(_Bogdanp$elm_time$Time_Date$setDay, day, _p52._0.date),
				offset: _p52._0.offset
			});
	});
var _Bogdanp$elm_time$Time_DateTime$addYears = F2(
	function (years, _p53) {
		var _p54 = _p53;
		return _Bogdanp$elm_time$Time_DateTime$DateTime(
			{
				date: A2(_Bogdanp$elm_time$Time_Date$addYears, years, _p54._0.date),
				offset: _p54._0.offset
			});
	});
var _Bogdanp$elm_time$Time_DateTime$addMonths = F2(
	function (months, _p55) {
		var _p56 = _p55;
		return _Bogdanp$elm_time$Time_DateTime$DateTime(
			{
				date: A2(_Bogdanp$elm_time$Time_Date$addMonths, months, _p56._0.date),
				offset: _p56._0.offset
			});
	});
var _Bogdanp$elm_time$Time_DateTime$addDays = F2(
	function (days, _p57) {
		var _p58 = _p57;
		return _Bogdanp$elm_time$Time_DateTime$DateTime(
			{
				date: A2(_Bogdanp$elm_time$Time_Date$addDays, days, _p58._0.date),
				offset: _p58._0.offset
			});
	});
var _Bogdanp$elm_time$Time_DateTime$addMilliseconds = F2(
	function (ms, _p59) {
		var _p60 = _p59;
		var total = ms + _p60._0.offset;
		var _p61 = function () {
			if (_elm_lang$core$Native_Utils.cmp(total, 0) < 0) {
				var offset = A2(_elm_lang$core$Basics$rem, total, _Bogdanp$elm_time$Time_Internal$dayMs);
				var days = 0 - (((_elm_lang$core$Basics$abs(total) / _Bogdanp$elm_time$Time_Internal$dayMs) | 0) + 1);
				return _elm_lang$core$Native_Utils.eq(offset, 0) ? {ctor: '_Tuple2', _0: days + 1, _1: 0} : {
					ctor: '_Tuple2',
					_0: days,
					_1: _Bogdanp$elm_time$Time_Internal$dayMs + A2(_elm_lang$core$Basics$rem, offset, _Bogdanp$elm_time$Time_Internal$dayMs)
				};
			} else {
				return {
					ctor: '_Tuple2',
					_0: (total / _Bogdanp$elm_time$Time_Internal$dayMs) | 0,
					_1: A2(_elm_lang$core$Basics$rem, total, _Bogdanp$elm_time$Time_Internal$dayMs)
				};
			}
		}();
		var days = _p61._0;
		var newOffset = _p61._1;
		return _Bogdanp$elm_time$Time_DateTime$DateTime(
			{
				date: A2(_Bogdanp$elm_time$Time_Date$addDays, days, _p60._0.date),
				offset: newOffset
			});
	});
var _Bogdanp$elm_time$Time_DateTime$addHours = F2(
	function (hours, time) {
		return A2(_Bogdanp$elm_time$Time_DateTime$addMilliseconds, hours * _Bogdanp$elm_time$Time_Internal$hourMs, time);
	});
var _Bogdanp$elm_time$Time_DateTime$addMinutes = F2(
	function (minutes, time) {
		return A2(_Bogdanp$elm_time$Time_DateTime$addMilliseconds, minutes * _Bogdanp$elm_time$Time_Internal$minuteMs, time);
	});
var _Bogdanp$elm_time$Time_DateTime$fromISO8601 = function (input) {
	var convert = function (_p62) {
		var _p63 = _p62;
		var _p69 = _p63._0._0;
		var _p68 = _p63._1._2;
		var _p67 = _p63._0._1;
		var _p66 = _p63._1._1;
		var _p65 = _p63._1._0;
		var _p64 = _p63._0._2;
		return (A3(_Bogdanp$elm_time$Time_Date$isValidDate, _p69, _p67, _p64) && A4(_Bogdanp$elm_time$Time_DateTime$isValidTime, _p65, _p66, _p68, 0)) ? _Bogdanp$elm_combine$Combine$succeed(
			A2(
				_Bogdanp$elm_time$Time_DateTime$addMinutes,
				0 - _p63._2,
				_Bogdanp$elm_time$Time_DateTime$dateTime(
					A7(_Bogdanp$elm_time$Time_Internal$DateTimeData, _p69, _p67, _p64, _p65, _p66, _p68, _p63._1._3)))) : _Bogdanp$elm_combine$Combine$fail('invalid date');
	};
	var minutes = A2(
		_Bogdanp$elm_combine$Combine_ops['<*>'],
		A2(
			_Bogdanp$elm_combine$Combine_ops['<*>'],
			A2(
				_Bogdanp$elm_combine$Combine_ops['<$>'],
				F3(
					function (s, h, m) {
						return ((s * h) * 60) + (s * m);
					}),
				_Bogdanp$elm_combine$Combine_Num$sign),
			A2(_Bogdanp$elm_time$Time_Internal$intRange, 0, 23)),
		A2(
			_Bogdanp$elm_combine$Combine_ops['*>'],
			_Bogdanp$elm_combine$Combine$string(':'),
			A2(_Bogdanp$elm_time$Time_Internal$intRange, 0, 59)));
	var offset = A2(
		_Bogdanp$elm_combine$Combine_ops['<|>'],
		A2(
			_Bogdanp$elm_combine$Combine_ops['<$'],
			0,
			_Bogdanp$elm_combine$Combine$string('Z')),
		minutes);
	var date = A2(
		_Bogdanp$elm_combine$Combine_ops['<*>'],
		A2(
			_Bogdanp$elm_combine$Combine_ops['<*>'],
			A2(
				_Bogdanp$elm_combine$Combine_ops['<$>'],
				F3(
					function (v0, v1, v2) {
						return {ctor: '_Tuple3', _0: v0, _1: v1, _2: v2};
					}),
				_Bogdanp$elm_combine$Combine_Num$int),
			A2(
				_Bogdanp$elm_combine$Combine_ops['*>'],
				_Bogdanp$elm_combine$Combine$string('-'),
				A2(_Bogdanp$elm_time$Time_Internal$intRange, 1, 12))),
		A2(
			_Bogdanp$elm_combine$Combine_ops['*>'],
			_Bogdanp$elm_combine$Combine$string('-'),
			A2(_Bogdanp$elm_time$Time_Internal$intRange, 1, 31)));
	var fraction = function () {
		var convert = F2(
			function (padding, _p70) {
				var _p71 = _p70;
				var _p73 = _p71._1;
				var _p72 = (_elm_lang$core$Native_Utils.cmp(_p73, 3) > 0) ? {
					ctor: '_Tuple2',
					_0: 1,
					_1: Math.pow(10, _p73 - 3)
				} : {
					ctor: '_Tuple2',
					_0: (1000 / Math.pow(10, _p73)) | 0,
					_1: 1
				};
				var multiplier = _p72._0;
				var remainder = _p72._1;
				return ((((_p71._0 * multiplier) / padding) | 0) / remainder) | 0;
			});
		var digits = A2(
			_Bogdanp$elm_combine$Combine_ops['<$>'],
			function (n) {
				return {
					ctor: '_Tuple2',
					_0: n,
					_1: _elm_lang$core$String$length(
						_elm_lang$core$Basics$toString(n))
				};
			},
			_Bogdanp$elm_combine$Combine_Num$int);
		var padding = A2(
			_Bogdanp$elm_combine$Combine_ops['<$>'],
			function (p) {
				return Math.pow(
					10,
					_elm_lang$core$String$length(p));
			},
			_Bogdanp$elm_combine$Combine$regex('0*'));
		return A2(
			_Bogdanp$elm_combine$Combine_ops['<*>'],
			A2(_Bogdanp$elm_combine$Combine_ops['<$>'], convert, padding),
			digits);
	}();
	var time = A2(
		_Bogdanp$elm_combine$Combine_ops['<*>'],
		A2(
			_Bogdanp$elm_combine$Combine_ops['<*>'],
			A2(
				_Bogdanp$elm_combine$Combine_ops['<*>'],
				A2(
					_Bogdanp$elm_combine$Combine_ops['<$>'],
					F4(
						function (v0, v1, v2, v3) {
							return {ctor: '_Tuple4', _0: v0, _1: v1, _2: v2, _3: v3};
						}),
					A2(
						_Bogdanp$elm_combine$Combine_ops['*>'],
						_Bogdanp$elm_combine$Combine$string('T'),
						A2(_Bogdanp$elm_time$Time_Internal$intRange, 0, 23))),
				A2(
					_Bogdanp$elm_combine$Combine_ops['*>'],
					_Bogdanp$elm_combine$Combine$string(':'),
					A2(_Bogdanp$elm_time$Time_Internal$intRange, 0, 59))),
			A2(
				_Bogdanp$elm_combine$Combine_ops['*>'],
				_Bogdanp$elm_combine$Combine$string(':'),
				A2(_Bogdanp$elm_time$Time_Internal$intRange, 0, 59))),
		A2(
			_Bogdanp$elm_combine$Combine$optional,
			0,
			A2(
				_Bogdanp$elm_combine$Combine_ops['*>'],
				_Bogdanp$elm_combine$Combine$regex('[,.]'),
				fraction)));
	var datetime = A2(
		_Bogdanp$elm_combine$Combine_ops['<*'],
		A2(
			_Bogdanp$elm_combine$Combine_ops['<*>'],
			A2(
				_Bogdanp$elm_combine$Combine_ops['<*>'],
				A2(
					_Bogdanp$elm_combine$Combine_ops['<$>'],
					F3(
						function (v0, v1, v2) {
							return {ctor: '_Tuple3', _0: v0, _1: v1, _2: v2};
						}),
					date),
				time),
			offset),
		_Bogdanp$elm_combine$Combine$end);
	var _p74 = A2(
		_Bogdanp$elm_combine$Combine$parse,
		A2(_Bogdanp$elm_combine$Combine_ops['>>='], datetime, convert),
		input);
	if (_p74.ctor === 'Ok') {
		return _elm_lang$core$Result$Ok(_p74._0._2);
	} else {
		var messages = A2(_elm_lang$core$String$join, ' or ', _p74._0._2);
		return _elm_lang$core$Result$Err(
			A2(
				_elm_lang$core$Basics_ops['++'],
				'Errors encountered at position ',
				A2(
					_elm_lang$core$Basics_ops['++'],
					_elm_lang$core$Basics$toString(_p74._0._1.position),
					A2(_elm_lang$core$Basics_ops['++'], ': ', messages))));
	}
};
var _Bogdanp$elm_time$Time_DateTime$addSeconds = F2(
	function (seconds, time) {
		return A2(_Bogdanp$elm_time$Time_DateTime$addMilliseconds, seconds * _Bogdanp$elm_time$Time_Internal$secondMs, time);
	});
var _Bogdanp$elm_time$Time_DateTime$fromTimestamp = function (timestamp) {
	return A2(
		_Bogdanp$elm_time$Time_DateTime$addMilliseconds,
		_elm_lang$core$Basics$round(timestamp),
		_Bogdanp$elm_time$Time_DateTime$epoch);
};

//import Native.List //

var _elm_lang$core$Native_Array = function() {

// A RRB-Tree has two distinct data types.
// Leaf -> "height"  is always 0
//         "table"   is an array of elements
// Node -> "height"  is always greater than 0
//         "table"   is an array of child nodes
//         "lengths" is an array of accumulated lengths of the child nodes

// M is the maximal table size. 32 seems fast. E is the allowed increase
// of search steps when concatting to find an index. Lower values will
// decrease balancing, but will increase search steps.
var M = 32;
var E = 2;

// An empty array.
var empty = {
	ctor: '_Array',
	height: 0,
	table: []
};


function get(i, array)
{
	if (i < 0 || i >= length(array))
	{
		throw new Error(
			'Index ' + i + ' is out of range. Check the length of ' +
			'your array first or use getMaybe or getWithDefault.');
	}
	return unsafeGet(i, array);
}


function unsafeGet(i, array)
{
	for (var x = array.height; x > 0; x--)
	{
		var slot = i >> (x * 5);
		while (array.lengths[slot] <= i)
		{
			slot++;
		}
		if (slot > 0)
		{
			i -= array.lengths[slot - 1];
		}
		array = array.table[slot];
	}
	return array.table[i];
}


// Sets the value at the index i. Only the nodes leading to i will get
// copied and updated.
function set(i, item, array)
{
	if (i < 0 || length(array) <= i)
	{
		return array;
	}
	return unsafeSet(i, item, array);
}


function unsafeSet(i, item, array)
{
	array = nodeCopy(array);

	if (array.height === 0)
	{
		array.table[i] = item;
	}
	else
	{
		var slot = getSlot(i, array);
		if (slot > 0)
		{
			i -= array.lengths[slot - 1];
		}
		array.table[slot] = unsafeSet(i, item, array.table[slot]);
	}
	return array;
}


function initialize(len, f)
{
	if (len <= 0)
	{
		return empty;
	}
	var h = Math.floor( Math.log(len) / Math.log(M) );
	return initialize_(f, h, 0, len);
}

function initialize_(f, h, from, to)
{
	if (h === 0)
	{
		var table = new Array((to - from) % (M + 1));
		for (var i = 0; i < table.length; i++)
		{
		  table[i] = f(from + i);
		}
		return {
			ctor: '_Array',
			height: 0,
			table: table
		};
	}

	var step = Math.pow(M, h);
	var table = new Array(Math.ceil((to - from) / step));
	var lengths = new Array(table.length);
	for (var i = 0; i < table.length; i++)
	{
		table[i] = initialize_(f, h - 1, from + (i * step), Math.min(from + ((i + 1) * step), to));
		lengths[i] = length(table[i]) + (i > 0 ? lengths[i-1] : 0);
	}
	return {
		ctor: '_Array',
		height: h,
		table: table,
		lengths: lengths
	};
}

function fromList(list)
{
	if (list.ctor === '[]')
	{
		return empty;
	}

	// Allocate M sized blocks (table) and write list elements to it.
	var table = new Array(M);
	var nodes = [];
	var i = 0;

	while (list.ctor !== '[]')
	{
		table[i] = list._0;
		list = list._1;
		i++;

		// table is full, so we can push a leaf containing it into the
		// next node.
		if (i === M)
		{
			var leaf = {
				ctor: '_Array',
				height: 0,
				table: table
			};
			fromListPush(leaf, nodes);
			table = new Array(M);
			i = 0;
		}
	}

	// Maybe there is something left on the table.
	if (i > 0)
	{
		var leaf = {
			ctor: '_Array',
			height: 0,
			table: table.splice(0, i)
		};
		fromListPush(leaf, nodes);
	}

	// Go through all of the nodes and eventually push them into higher nodes.
	for (var h = 0; h < nodes.length - 1; h++)
	{
		if (nodes[h].table.length > 0)
		{
			fromListPush(nodes[h], nodes);
		}
	}

	var head = nodes[nodes.length - 1];
	if (head.height > 0 && head.table.length === 1)
	{
		return head.table[0];
	}
	else
	{
		return head;
	}
}

// Push a node into a higher node as a child.
function fromListPush(toPush, nodes)
{
	var h = toPush.height;

	// Maybe the node on this height does not exist.
	if (nodes.length === h)
	{
		var node = {
			ctor: '_Array',
			height: h + 1,
			table: [],
			lengths: []
		};
		nodes.push(node);
	}

	nodes[h].table.push(toPush);
	var len = length(toPush);
	if (nodes[h].lengths.length > 0)
	{
		len += nodes[h].lengths[nodes[h].lengths.length - 1];
	}
	nodes[h].lengths.push(len);

	if (nodes[h].table.length === M)
	{
		fromListPush(nodes[h], nodes);
		nodes[h] = {
			ctor: '_Array',
			height: h + 1,
			table: [],
			lengths: []
		};
	}
}

// Pushes an item via push_ to the bottom right of a tree.
function push(item, a)
{
	var pushed = push_(item, a);
	if (pushed !== null)
	{
		return pushed;
	}

	var newTree = create(item, a.height);
	return siblise(a, newTree);
}

// Recursively tries to push an item to the bottom-right most
// tree possible. If there is no space left for the item,
// null will be returned.
function push_(item, a)
{
	// Handle resursion stop at leaf level.
	if (a.height === 0)
	{
		if (a.table.length < M)
		{
			var newA = {
				ctor: '_Array',
				height: 0,
				table: a.table.slice()
			};
			newA.table.push(item);
			return newA;
		}
		else
		{
		  return null;
		}
	}

	// Recursively push
	var pushed = push_(item, botRight(a));

	// There was space in the bottom right tree, so the slot will
	// be updated.
	if (pushed !== null)
	{
		var newA = nodeCopy(a);
		newA.table[newA.table.length - 1] = pushed;
		newA.lengths[newA.lengths.length - 1]++;
		return newA;
	}

	// When there was no space left, check if there is space left
	// for a new slot with a tree which contains only the item
	// at the bottom.
	if (a.table.length < M)
	{
		var newSlot = create(item, a.height - 1);
		var newA = nodeCopy(a);
		newA.table.push(newSlot);
		newA.lengths.push(newA.lengths[newA.lengths.length - 1] + length(newSlot));
		return newA;
	}
	else
	{
		return null;
	}
}

// Converts an array into a list of elements.
function toList(a)
{
	return toList_(_elm_lang$core$Native_List.Nil, a);
}

function toList_(list, a)
{
	for (var i = a.table.length - 1; i >= 0; i--)
	{
		list =
			a.height === 0
				? _elm_lang$core$Native_List.Cons(a.table[i], list)
				: toList_(list, a.table[i]);
	}
	return list;
}

// Maps a function over the elements of an array.
function map(f, a)
{
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: new Array(a.table.length)
	};
	if (a.height > 0)
	{
		newA.lengths = a.lengths;
	}
	for (var i = 0; i < a.table.length; i++)
	{
		newA.table[i] =
			a.height === 0
				? f(a.table[i])
				: map(f, a.table[i]);
	}
	return newA;
}

// Maps a function over the elements with their index as first argument.
function indexedMap(f, a)
{
	return indexedMap_(f, a, 0);
}

function indexedMap_(f, a, from)
{
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: new Array(a.table.length)
	};
	if (a.height > 0)
	{
		newA.lengths = a.lengths;
	}
	for (var i = 0; i < a.table.length; i++)
	{
		newA.table[i] =
			a.height === 0
				? A2(f, from + i, a.table[i])
				: indexedMap_(f, a.table[i], i == 0 ? from : from + a.lengths[i - 1]);
	}
	return newA;
}

function foldl(f, b, a)
{
	if (a.height === 0)
	{
		for (var i = 0; i < a.table.length; i++)
		{
			b = A2(f, a.table[i], b);
		}
	}
	else
	{
		for (var i = 0; i < a.table.length; i++)
		{
			b = foldl(f, b, a.table[i]);
		}
	}
	return b;
}

function foldr(f, b, a)
{
	if (a.height === 0)
	{
		for (var i = a.table.length; i--; )
		{
			b = A2(f, a.table[i], b);
		}
	}
	else
	{
		for (var i = a.table.length; i--; )
		{
			b = foldr(f, b, a.table[i]);
		}
	}
	return b;
}

// TODO: currently, it slices the right, then the left. This can be
// optimized.
function slice(from, to, a)
{
	if (from < 0)
	{
		from += length(a);
	}
	if (to < 0)
	{
		to += length(a);
	}
	return sliceLeft(from, sliceRight(to, a));
}

function sliceRight(to, a)
{
	if (to === length(a))
	{
		return a;
	}

	// Handle leaf level.
	if (a.height === 0)
	{
		var newA = { ctor:'_Array', height:0 };
		newA.table = a.table.slice(0, to);
		return newA;
	}

	// Slice the right recursively.
	var right = getSlot(to, a);
	var sliced = sliceRight(to - (right > 0 ? a.lengths[right - 1] : 0), a.table[right]);

	// Maybe the a node is not even needed, as sliced contains the whole slice.
	if (right === 0)
	{
		return sliced;
	}

	// Create new node.
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: a.table.slice(0, right),
		lengths: a.lengths.slice(0, right)
	};
	if (sliced.table.length > 0)
	{
		newA.table[right] = sliced;
		newA.lengths[right] = length(sliced) + (right > 0 ? newA.lengths[right - 1] : 0);
	}
	return newA;
}

function sliceLeft(from, a)
{
	if (from === 0)
	{
		return a;
	}

	// Handle leaf level.
	if (a.height === 0)
	{
		var newA = { ctor:'_Array', height:0 };
		newA.table = a.table.slice(from, a.table.length + 1);
		return newA;
	}

	// Slice the left recursively.
	var left = getSlot(from, a);
	var sliced = sliceLeft(from - (left > 0 ? a.lengths[left - 1] : 0), a.table[left]);

	// Maybe the a node is not even needed, as sliced contains the whole slice.
	if (left === a.table.length - 1)
	{
		return sliced;
	}

	// Create new node.
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: a.table.slice(left, a.table.length + 1),
		lengths: new Array(a.table.length - left)
	};
	newA.table[0] = sliced;
	var len = 0;
	for (var i = 0; i < newA.table.length; i++)
	{
		len += length(newA.table[i]);
		newA.lengths[i] = len;
	}

	return newA;
}

// Appends two trees.
function append(a,b)
{
	if (a.table.length === 0)
	{
		return b;
	}
	if (b.table.length === 0)
	{
		return a;
	}

	var c = append_(a, b);

	// Check if both nodes can be crunshed together.
	if (c[0].table.length + c[1].table.length <= M)
	{
		if (c[0].table.length === 0)
		{
			return c[1];
		}
		if (c[1].table.length === 0)
		{
			return c[0];
		}

		// Adjust .table and .lengths
		c[0].table = c[0].table.concat(c[1].table);
		if (c[0].height > 0)
		{
			var len = length(c[0]);
			for (var i = 0; i < c[1].lengths.length; i++)
			{
				c[1].lengths[i] += len;
			}
			c[0].lengths = c[0].lengths.concat(c[1].lengths);
		}

		return c[0];
	}

	if (c[0].height > 0)
	{
		var toRemove = calcToRemove(a, b);
		if (toRemove > E)
		{
			c = shuffle(c[0], c[1], toRemove);
		}
	}

	return siblise(c[0], c[1]);
}

// Returns an array of two nodes; right and left. One node _may_ be empty.
function append_(a, b)
{
	if (a.height === 0 && b.height === 0)
	{
		return [a, b];
	}

	if (a.height !== 1 || b.height !== 1)
	{
		if (a.height === b.height)
		{
			a = nodeCopy(a);
			b = nodeCopy(b);
			var appended = append_(botRight(a), botLeft(b));

			insertRight(a, appended[1]);
			insertLeft(b, appended[0]);
		}
		else if (a.height > b.height)
		{
			a = nodeCopy(a);
			var appended = append_(botRight(a), b);

			insertRight(a, appended[0]);
			b = parentise(appended[1], appended[1].height + 1);
		}
		else
		{
			b = nodeCopy(b);
			var appended = append_(a, botLeft(b));

			var left = appended[0].table.length === 0 ? 0 : 1;
			var right = left === 0 ? 1 : 0;
			insertLeft(b, appended[left]);
			a = parentise(appended[right], appended[right].height + 1);
		}
	}

	// Check if balancing is needed and return based on that.
	if (a.table.length === 0 || b.table.length === 0)
	{
		return [a, b];
	}

	var toRemove = calcToRemove(a, b);
	if (toRemove <= E)
	{
		return [a, b];
	}
	return shuffle(a, b, toRemove);
}

// Helperfunctions for append_. Replaces a child node at the side of the parent.
function insertRight(parent, node)
{
	var index = parent.table.length - 1;
	parent.table[index] = node;
	parent.lengths[index] = length(node);
	parent.lengths[index] += index > 0 ? parent.lengths[index - 1] : 0;
}

function insertLeft(parent, node)
{
	if (node.table.length > 0)
	{
		parent.table[0] = node;
		parent.lengths[0] = length(node);

		var len = length(parent.table[0]);
		for (var i = 1; i < parent.lengths.length; i++)
		{
			len += length(parent.table[i]);
			parent.lengths[i] = len;
		}
	}
	else
	{
		parent.table.shift();
		for (var i = 1; i < parent.lengths.length; i++)
		{
			parent.lengths[i] = parent.lengths[i] - parent.lengths[0];
		}
		parent.lengths.shift();
	}
}

// Returns the extra search steps for E. Refer to the paper.
function calcToRemove(a, b)
{
	var subLengths = 0;
	for (var i = 0; i < a.table.length; i++)
	{
		subLengths += a.table[i].table.length;
	}
	for (var i = 0; i < b.table.length; i++)
	{
		subLengths += b.table[i].table.length;
	}

	var toRemove = a.table.length + b.table.length;
	return toRemove - (Math.floor((subLengths - 1) / M) + 1);
}

// get2, set2 and saveSlot are helpers for accessing elements over two arrays.
function get2(a, b, index)
{
	return index < a.length
		? a[index]
		: b[index - a.length];
}

function set2(a, b, index, value)
{
	if (index < a.length)
	{
		a[index] = value;
	}
	else
	{
		b[index - a.length] = value;
	}
}

function saveSlot(a, b, index, slot)
{
	set2(a.table, b.table, index, slot);

	var l = (index === 0 || index === a.lengths.length)
		? 0
		: get2(a.lengths, a.lengths, index - 1);

	set2(a.lengths, b.lengths, index, l + length(slot));
}

// Creates a node or leaf with a given length at their arrays for perfomance.
// Is only used by shuffle.
function createNode(h, length)
{
	if (length < 0)
	{
		length = 0;
	}
	var a = {
		ctor: '_Array',
		height: h,
		table: new Array(length)
	};
	if (h > 0)
	{
		a.lengths = new Array(length);
	}
	return a;
}

// Returns an array of two balanced nodes.
function shuffle(a, b, toRemove)
{
	var newA = createNode(a.height, Math.min(M, a.table.length + b.table.length - toRemove));
	var newB = createNode(a.height, newA.table.length - (a.table.length + b.table.length - toRemove));

	// Skip the slots with size M. More precise: copy the slot references
	// to the new node
	var read = 0;
	while (get2(a.table, b.table, read).table.length % M === 0)
	{
		set2(newA.table, newB.table, read, get2(a.table, b.table, read));
		set2(newA.lengths, newB.lengths, read, get2(a.lengths, b.lengths, read));
		read++;
	}

	// Pulling items from left to right, caching in a slot before writing
	// it into the new nodes.
	var write = read;
	var slot = new createNode(a.height - 1, 0);
	var from = 0;

	// If the current slot is still containing data, then there will be at
	// least one more write, so we do not break this loop yet.
	while (read - write - (slot.table.length > 0 ? 1 : 0) < toRemove)
	{
		// Find out the max possible items for copying.
		var source = get2(a.table, b.table, read);
		var to = Math.min(M - slot.table.length, source.table.length);

		// Copy and adjust size table.
		slot.table = slot.table.concat(source.table.slice(from, to));
		if (slot.height > 0)
		{
			var len = slot.lengths.length;
			for (var i = len; i < len + to - from; i++)
			{
				slot.lengths[i] = length(slot.table[i]);
				slot.lengths[i] += (i > 0 ? slot.lengths[i - 1] : 0);
			}
		}

		from += to;

		// Only proceed to next slots[i] if the current one was
		// fully copied.
		if (source.table.length <= to)
		{
			read++; from = 0;
		}

		// Only create a new slot if the current one is filled up.
		if (slot.table.length === M)
		{
			saveSlot(newA, newB, write, slot);
			slot = createNode(a.height - 1, 0);
			write++;
		}
	}

	// Cleanup after the loop. Copy the last slot into the new nodes.
	if (slot.table.length > 0)
	{
		saveSlot(newA, newB, write, slot);
		write++;
	}

	// Shift the untouched slots to the left
	while (read < a.table.length + b.table.length )
	{
		saveSlot(newA, newB, write, get2(a.table, b.table, read));
		read++;
		write++;
	}

	return [newA, newB];
}

// Navigation functions
function botRight(a)
{
	return a.table[a.table.length - 1];
}
function botLeft(a)
{
	return a.table[0];
}

// Copies a node for updating. Note that you should not use this if
// only updating only one of "table" or "lengths" for performance reasons.
function nodeCopy(a)
{
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: a.table.slice()
	};
	if (a.height > 0)
	{
		newA.lengths = a.lengths.slice();
	}
	return newA;
}

// Returns how many items are in the tree.
function length(array)
{
	if (array.height === 0)
	{
		return array.table.length;
	}
	else
	{
		return array.lengths[array.lengths.length - 1];
	}
}

// Calculates in which slot of "table" the item probably is, then
// find the exact slot via forward searching in  "lengths". Returns the index.
function getSlot(i, a)
{
	var slot = i >> (5 * a.height);
	while (a.lengths[slot] <= i)
	{
		slot++;
	}
	return slot;
}

// Recursively creates a tree with a given height containing
// only the given item.
function create(item, h)
{
	if (h === 0)
	{
		return {
			ctor: '_Array',
			height: 0,
			table: [item]
		};
	}
	return {
		ctor: '_Array',
		height: h,
		table: [create(item, h - 1)],
		lengths: [1]
	};
}

// Recursively creates a tree that contains the given tree.
function parentise(tree, h)
{
	if (h === tree.height)
	{
		return tree;
	}

	return {
		ctor: '_Array',
		height: h,
		table: [parentise(tree, h - 1)],
		lengths: [length(tree)]
	};
}

// Emphasizes blood brotherhood beneath two trees.
function siblise(a, b)
{
	return {
		ctor: '_Array',
		height: a.height + 1,
		table: [a, b],
		lengths: [length(a), length(a) + length(b)]
	};
}

function toJSArray(a)
{
	var jsArray = new Array(length(a));
	toJSArray_(jsArray, 0, a);
	return jsArray;
}

function toJSArray_(jsArray, i, a)
{
	for (var t = 0; t < a.table.length; t++)
	{
		if (a.height === 0)
		{
			jsArray[i + t] = a.table[t];
		}
		else
		{
			var inc = t === 0 ? 0 : a.lengths[t - 1];
			toJSArray_(jsArray, i + inc, a.table[t]);
		}
	}
}

function fromJSArray(jsArray)
{
	if (jsArray.length === 0)
	{
		return empty;
	}
	var h = Math.floor(Math.log(jsArray.length) / Math.log(M));
	return fromJSArray_(jsArray, h, 0, jsArray.length);
}

function fromJSArray_(jsArray, h, from, to)
{
	if (h === 0)
	{
		return {
			ctor: '_Array',
			height: 0,
			table: jsArray.slice(from, to)
		};
	}

	var step = Math.pow(M, h);
	var table = new Array(Math.ceil((to - from) / step));
	var lengths = new Array(table.length);
	for (var i = 0; i < table.length; i++)
	{
		table[i] = fromJSArray_(jsArray, h - 1, from + (i * step), Math.min(from + ((i + 1) * step), to));
		lengths[i] = length(table[i]) + (i > 0 ? lengths[i - 1] : 0);
	}
	return {
		ctor: '_Array',
		height: h,
		table: table,
		lengths: lengths
	};
}

return {
	empty: empty,
	fromList: fromList,
	toList: toList,
	initialize: F2(initialize),
	append: F2(append),
	push: F2(push),
	slice: F3(slice),
	get: F2(get),
	set: F3(set),
	map: F2(map),
	indexedMap: F2(indexedMap),
	foldl: F3(foldl),
	foldr: F3(foldr),
	length: length,

	toJSArray: toJSArray,
	fromJSArray: fromJSArray
};

}();
var _elm_lang$core$Array$append = _elm_lang$core$Native_Array.append;
var _elm_lang$core$Array$length = _elm_lang$core$Native_Array.length;
var _elm_lang$core$Array$isEmpty = function (array) {
	return _elm_lang$core$Native_Utils.eq(
		_elm_lang$core$Array$length(array),
		0);
};
var _elm_lang$core$Array$slice = _elm_lang$core$Native_Array.slice;
var _elm_lang$core$Array$set = _elm_lang$core$Native_Array.set;
var _elm_lang$core$Array$get = F2(
	function (i, array) {
		return ((_elm_lang$core$Native_Utils.cmp(0, i) < 1) && (_elm_lang$core$Native_Utils.cmp(
			i,
			_elm_lang$core$Native_Array.length(array)) < 0)) ? _elm_lang$core$Maybe$Just(
			A2(_elm_lang$core$Native_Array.get, i, array)) : _elm_lang$core$Maybe$Nothing;
	});
var _elm_lang$core$Array$push = _elm_lang$core$Native_Array.push;
var _elm_lang$core$Array$empty = _elm_lang$core$Native_Array.empty;
var _elm_lang$core$Array$filter = F2(
	function (isOkay, arr) {
		var update = F2(
			function (x, xs) {
				return isOkay(x) ? A2(_elm_lang$core$Native_Array.push, x, xs) : xs;
			});
		return A3(_elm_lang$core$Native_Array.foldl, update, _elm_lang$core$Native_Array.empty, arr);
	});
var _elm_lang$core$Array$foldr = _elm_lang$core$Native_Array.foldr;
var _elm_lang$core$Array$foldl = _elm_lang$core$Native_Array.foldl;
var _elm_lang$core$Array$indexedMap = _elm_lang$core$Native_Array.indexedMap;
var _elm_lang$core$Array$map = _elm_lang$core$Native_Array.map;
var _elm_lang$core$Array$toIndexedList = function (array) {
	return A3(
		_elm_lang$core$List$map2,
		F2(
			function (v0, v1) {
				return {ctor: '_Tuple2', _0: v0, _1: v1};
			}),
		A2(
			_elm_lang$core$List$range,
			0,
			_elm_lang$core$Native_Array.length(array) - 1),
		_elm_lang$core$Native_Array.toList(array));
};
var _elm_lang$core$Array$toList = _elm_lang$core$Native_Array.toList;
var _elm_lang$core$Array$fromList = _elm_lang$core$Native_Array.fromList;
var _elm_lang$core$Array$initialize = _elm_lang$core$Native_Array.initialize;
var _elm_lang$core$Array$repeat = F2(
	function (n, e) {
		return A2(
			_elm_lang$core$Array$initialize,
			n,
			_elm_lang$core$Basics$always(e));
	});
var _elm_lang$core$Array$Array = {ctor: 'Array'};

//import Maybe, Native.Array, Native.List, Native.Utils, Result //

var _elm_lang$core$Native_Json = function() {


// CORE DECODERS

function succeed(msg)
{
	return {
		ctor: '<decoder>',
		tag: 'succeed',
		msg: msg
	};
}

function fail(msg)
{
	return {
		ctor: '<decoder>',
		tag: 'fail',
		msg: msg
	};
}

function decodePrimitive(tag)
{
	return {
		ctor: '<decoder>',
		tag: tag
	};
}

function decodeContainer(tag, decoder)
{
	return {
		ctor: '<decoder>',
		tag: tag,
		decoder: decoder
	};
}

function decodeNull(value)
{
	return {
		ctor: '<decoder>',
		tag: 'null',
		value: value
	};
}

function decodeField(field, decoder)
{
	return {
		ctor: '<decoder>',
		tag: 'field',
		field: field,
		decoder: decoder
	};
}

function decodeIndex(index, decoder)
{
	return {
		ctor: '<decoder>',
		tag: 'index',
		index: index,
		decoder: decoder
	};
}

function decodeKeyValuePairs(decoder)
{
	return {
		ctor: '<decoder>',
		tag: 'key-value',
		decoder: decoder
	};
}

function mapMany(f, decoders)
{
	return {
		ctor: '<decoder>',
		tag: 'map-many',
		func: f,
		decoders: decoders
	};
}

function andThen(callback, decoder)
{
	return {
		ctor: '<decoder>',
		tag: 'andThen',
		decoder: decoder,
		callback: callback
	};
}

function oneOf(decoders)
{
	return {
		ctor: '<decoder>',
		tag: 'oneOf',
		decoders: decoders
	};
}


// DECODING OBJECTS

function map1(f, d1)
{
	return mapMany(f, [d1]);
}

function map2(f, d1, d2)
{
	return mapMany(f, [d1, d2]);
}

function map3(f, d1, d2, d3)
{
	return mapMany(f, [d1, d2, d3]);
}

function map4(f, d1, d2, d3, d4)
{
	return mapMany(f, [d1, d2, d3, d4]);
}

function map5(f, d1, d2, d3, d4, d5)
{
	return mapMany(f, [d1, d2, d3, d4, d5]);
}

function map6(f, d1, d2, d3, d4, d5, d6)
{
	return mapMany(f, [d1, d2, d3, d4, d5, d6]);
}

function map7(f, d1, d2, d3, d4, d5, d6, d7)
{
	return mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
}

function map8(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
}


// DECODE HELPERS

function ok(value)
{
	return { tag: 'ok', value: value };
}

function badPrimitive(type, value)
{
	return { tag: 'primitive', type: type, value: value };
}

function badIndex(index, nestedProblems)
{
	return { tag: 'index', index: index, rest: nestedProblems };
}

function badField(field, nestedProblems)
{
	return { tag: 'field', field: field, rest: nestedProblems };
}

function badIndex(index, nestedProblems)
{
	return { tag: 'index', index: index, rest: nestedProblems };
}

function badOneOf(problems)
{
	return { tag: 'oneOf', problems: problems };
}

function bad(msg)
{
	return { tag: 'fail', msg: msg };
}

function badToString(problem)
{
	var context = '_';
	while (problem)
	{
		switch (problem.tag)
		{
			case 'primitive':
				return 'Expecting ' + problem.type
					+ (context === '_' ? '' : ' at ' + context)
					+ ' but instead got: ' + jsToString(problem.value);

			case 'index':
				context += '[' + problem.index + ']';
				problem = problem.rest;
				break;

			case 'field':
				context += '.' + problem.field;
				problem = problem.rest;
				break;

			case 'oneOf':
				var problems = problem.problems;
				for (var i = 0; i < problems.length; i++)
				{
					problems[i] = badToString(problems[i]);
				}
				return 'I ran into the following problems'
					+ (context === '_' ? '' : ' at ' + context)
					+ ':\n\n' + problems.join('\n');

			case 'fail':
				return 'I ran into a `fail` decoder'
					+ (context === '_' ? '' : ' at ' + context)
					+ ': ' + problem.msg;
		}
	}
}

function jsToString(value)
{
	return value === undefined
		? 'undefined'
		: JSON.stringify(value);
}


// DECODE

function runOnString(decoder, string)
{
	var json;
	try
	{
		json = JSON.parse(string);
	}
	catch (e)
	{
		return _elm_lang$core$Result$Err('Given an invalid JSON: ' + e.message);
	}
	return run(decoder, json);
}

function run(decoder, value)
{
	var result = runHelp(decoder, value);
	return (result.tag === 'ok')
		? _elm_lang$core$Result$Ok(result.value)
		: _elm_lang$core$Result$Err(badToString(result));
}

function runHelp(decoder, value)
{
	switch (decoder.tag)
	{
		case 'bool':
			return (typeof value === 'boolean')
				? ok(value)
				: badPrimitive('a Bool', value);

		case 'int':
			if (typeof value !== 'number') {
				return badPrimitive('an Int', value);
			}

			if (-2147483647 < value && value < 2147483647 && (value | 0) === value) {
				return ok(value);
			}

			if (isFinite(value) && !(value % 1)) {
				return ok(value);
			}

			return badPrimitive('an Int', value);

		case 'float':
			return (typeof value === 'number')
				? ok(value)
				: badPrimitive('a Float', value);

		case 'string':
			return (typeof value === 'string')
				? ok(value)
				: (value instanceof String)
					? ok(value + '')
					: badPrimitive('a String', value);

		case 'null':
			return (value === null)
				? ok(decoder.value)
				: badPrimitive('null', value);

		case 'value':
			return ok(value);

		case 'list':
			if (!(value instanceof Array))
			{
				return badPrimitive('a List', value);
			}

			var list = _elm_lang$core$Native_List.Nil;
			for (var i = value.length; i--; )
			{
				var result = runHelp(decoder.decoder, value[i]);
				if (result.tag !== 'ok')
				{
					return badIndex(i, result)
				}
				list = _elm_lang$core$Native_List.Cons(result.value, list);
			}
			return ok(list);

		case 'array':
			if (!(value instanceof Array))
			{
				return badPrimitive('an Array', value);
			}

			var len = value.length;
			var array = new Array(len);
			for (var i = len; i--; )
			{
				var result = runHelp(decoder.decoder, value[i]);
				if (result.tag !== 'ok')
				{
					return badIndex(i, result);
				}
				array[i] = result.value;
			}
			return ok(_elm_lang$core$Native_Array.fromJSArray(array));

		case 'maybe':
			var result = runHelp(decoder.decoder, value);
			return (result.tag === 'ok')
				? ok(_elm_lang$core$Maybe$Just(result.value))
				: ok(_elm_lang$core$Maybe$Nothing);

		case 'field':
			var field = decoder.field;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return badPrimitive('an object with a field named `' + field + '`', value);
			}

			var result = runHelp(decoder.decoder, value[field]);
			return (result.tag === 'ok') ? result : badField(field, result);

		case 'index':
			var index = decoder.index;
			if (!(value instanceof Array))
			{
				return badPrimitive('an array', value);
			}
			if (index >= value.length)
			{
				return badPrimitive('a longer array. Need index ' + index + ' but there are only ' + value.length + ' entries', value);
			}

			var result = runHelp(decoder.decoder, value[index]);
			return (result.tag === 'ok') ? result : badIndex(index, result);

		case 'key-value':
			if (typeof value !== 'object' || value === null || value instanceof Array)
			{
				return badPrimitive('an object', value);
			}

			var keyValuePairs = _elm_lang$core$Native_List.Nil;
			for (var key in value)
			{
				var result = runHelp(decoder.decoder, value[key]);
				if (result.tag !== 'ok')
				{
					return badField(key, result);
				}
				var pair = _elm_lang$core$Native_Utils.Tuple2(key, result.value);
				keyValuePairs = _elm_lang$core$Native_List.Cons(pair, keyValuePairs);
			}
			return ok(keyValuePairs);

		case 'map-many':
			var answer = decoder.func;
			var decoders = decoder.decoders;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = runHelp(decoders[i], value);
				if (result.tag !== 'ok')
				{
					return result;
				}
				answer = answer(result.value);
			}
			return ok(answer);

		case 'andThen':
			var result = runHelp(decoder.decoder, value);
			return (result.tag !== 'ok')
				? result
				: runHelp(decoder.callback(result.value), value);

		case 'oneOf':
			var errors = [];
			var temp = decoder.decoders;
			while (temp.ctor !== '[]')
			{
				var result = runHelp(temp._0, value);

				if (result.tag === 'ok')
				{
					return result;
				}

				errors.push(result);

				temp = temp._1;
			}
			return badOneOf(errors);

		case 'fail':
			return bad(decoder.msg);

		case 'succeed':
			return ok(decoder.msg);
	}
}


// EQUALITY

function equality(a, b)
{
	if (a === b)
	{
		return true;
	}

	if (a.tag !== b.tag)
	{
		return false;
	}

	switch (a.tag)
	{
		case 'succeed':
		case 'fail':
			return a.msg === b.msg;

		case 'bool':
		case 'int':
		case 'float':
		case 'string':
		case 'value':
			return true;

		case 'null':
			return a.value === b.value;

		case 'list':
		case 'array':
		case 'maybe':
		case 'key-value':
			return equality(a.decoder, b.decoder);

		case 'field':
			return a.field === b.field && equality(a.decoder, b.decoder);

		case 'index':
			return a.index === b.index && equality(a.decoder, b.decoder);

		case 'map-many':
			if (a.func !== b.func)
			{
				return false;
			}
			return listEquality(a.decoders, b.decoders);

		case 'andThen':
			return a.callback === b.callback && equality(a.decoder, b.decoder);

		case 'oneOf':
			return listEquality(a.decoders, b.decoders);
	}
}

function listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

function encode(indentLevel, value)
{
	return JSON.stringify(value, null, indentLevel);
}

function identity(value)
{
	return value;
}

function encodeObject(keyValuePairs)
{
	var obj = {};
	while (keyValuePairs.ctor !== '[]')
	{
		var pair = keyValuePairs._0;
		obj[pair._0] = pair._1;
		keyValuePairs = keyValuePairs._1;
	}
	return obj;
}

return {
	encode: F2(encode),
	runOnString: F2(runOnString),
	run: F2(run),

	decodeNull: decodeNull,
	decodePrimitive: decodePrimitive,
	decodeContainer: F2(decodeContainer),

	decodeField: F2(decodeField),
	decodeIndex: F2(decodeIndex),

	map1: F2(map1),
	map2: F3(map2),
	map3: F4(map3),
	map4: F5(map4),
	map5: F6(map5),
	map6: F7(map6),
	map7: F8(map7),
	map8: F9(map8),
	decodeKeyValuePairs: decodeKeyValuePairs,

	andThen: F2(andThen),
	fail: fail,
	succeed: succeed,
	oneOf: oneOf,

	identity: identity,
	encodeNull: null,
	encodeArray: _elm_lang$core$Native_Array.toJSArray,
	encodeList: _elm_lang$core$Native_List.toArray,
	encodeObject: encodeObject,

	equality: equality
};

}();

var _elm_lang$core$Json_Encode$list = _elm_lang$core$Native_Json.encodeList;
var _elm_lang$core$Json_Encode$array = _elm_lang$core$Native_Json.encodeArray;
var _elm_lang$core$Json_Encode$object = _elm_lang$core$Native_Json.encodeObject;
var _elm_lang$core$Json_Encode$null = _elm_lang$core$Native_Json.encodeNull;
var _elm_lang$core$Json_Encode$bool = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$float = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$int = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$string = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$encode = _elm_lang$core$Native_Json.encode;
var _elm_lang$core$Json_Encode$Value = {ctor: 'Value'};

var _elm_lang$core$Json_Decode$null = _elm_lang$core$Native_Json.decodeNull;
var _elm_lang$core$Json_Decode$value = _elm_lang$core$Native_Json.decodePrimitive('value');
var _elm_lang$core$Json_Decode$andThen = _elm_lang$core$Native_Json.andThen;
var _elm_lang$core$Json_Decode$fail = _elm_lang$core$Native_Json.fail;
var _elm_lang$core$Json_Decode$succeed = _elm_lang$core$Native_Json.succeed;
var _elm_lang$core$Json_Decode$lazy = function (thunk) {
	return A2(
		_elm_lang$core$Json_Decode$andThen,
		thunk,
		_elm_lang$core$Json_Decode$succeed(
			{ctor: '_Tuple0'}));
};
var _elm_lang$core$Json_Decode$decodeValue = _elm_lang$core$Native_Json.run;
var _elm_lang$core$Json_Decode$decodeString = _elm_lang$core$Native_Json.runOnString;
var _elm_lang$core$Json_Decode$map8 = _elm_lang$core$Native_Json.map8;
var _elm_lang$core$Json_Decode$map7 = _elm_lang$core$Native_Json.map7;
var _elm_lang$core$Json_Decode$map6 = _elm_lang$core$Native_Json.map6;
var _elm_lang$core$Json_Decode$map5 = _elm_lang$core$Native_Json.map5;
var _elm_lang$core$Json_Decode$map4 = _elm_lang$core$Native_Json.map4;
var _elm_lang$core$Json_Decode$map3 = _elm_lang$core$Native_Json.map3;
var _elm_lang$core$Json_Decode$map2 = _elm_lang$core$Native_Json.map2;
var _elm_lang$core$Json_Decode$map = _elm_lang$core$Native_Json.map1;
var _elm_lang$core$Json_Decode$oneOf = _elm_lang$core$Native_Json.oneOf;
var _elm_lang$core$Json_Decode$maybe = function (decoder) {
	return A2(_elm_lang$core$Native_Json.decodeContainer, 'maybe', decoder);
};
var _elm_lang$core$Json_Decode$index = _elm_lang$core$Native_Json.decodeIndex;
var _elm_lang$core$Json_Decode$field = _elm_lang$core$Native_Json.decodeField;
var _elm_lang$core$Json_Decode$at = F2(
	function (fields, decoder) {
		return A3(_elm_lang$core$List$foldr, _elm_lang$core$Json_Decode$field, decoder, fields);
	});
var _elm_lang$core$Json_Decode$keyValuePairs = _elm_lang$core$Native_Json.decodeKeyValuePairs;
var _elm_lang$core$Json_Decode$dict = function (decoder) {
	return A2(
		_elm_lang$core$Json_Decode$map,
		_elm_lang$core$Dict$fromList,
		_elm_lang$core$Json_Decode$keyValuePairs(decoder));
};
var _elm_lang$core$Json_Decode$array = function (decoder) {
	return A2(_elm_lang$core$Native_Json.decodeContainer, 'array', decoder);
};
var _elm_lang$core$Json_Decode$list = function (decoder) {
	return A2(_elm_lang$core$Native_Json.decodeContainer, 'list', decoder);
};
var _elm_lang$core$Json_Decode$nullable = function (decoder) {
	return _elm_lang$core$Json_Decode$oneOf(
		{
			ctor: '::',
			_0: _elm_lang$core$Json_Decode$null(_elm_lang$core$Maybe$Nothing),
			_1: {
				ctor: '::',
				_0: A2(_elm_lang$core$Json_Decode$map, _elm_lang$core$Maybe$Just, decoder),
				_1: {ctor: '[]'}
			}
		});
};
var _elm_lang$core$Json_Decode$float = _elm_lang$core$Native_Json.decodePrimitive('float');
var _elm_lang$core$Json_Decode$int = _elm_lang$core$Native_Json.decodePrimitive('int');
var _elm_lang$core$Json_Decode$bool = _elm_lang$core$Native_Json.decodePrimitive('bool');
var _elm_lang$core$Json_Decode$string = _elm_lang$core$Native_Json.decodePrimitive('string');
var _elm_lang$core$Json_Decode$Decoder = {ctor: 'Decoder'};

var _NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$decode = _elm_lang$core$Json_Decode$succeed;
var _NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$resolve = _elm_lang$core$Json_Decode$andThen(_elm_lang$core$Basics$identity);
var _NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$custom = _elm_lang$core$Json_Decode$map2(
	F2(
		function (x, y) {
			return y(x);
		}));
var _NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$hardcoded = function (_p0) {
	return _NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$custom(
		_elm_lang$core$Json_Decode$succeed(_p0));
};
var _NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$optionalDecoder = F3(
	function (pathDecoder, valDecoder, fallback) {
		var nullOr = function (decoder) {
			return _elm_lang$core$Json_Decode$oneOf(
				{
					ctor: '::',
					_0: decoder,
					_1: {
						ctor: '::',
						_0: _elm_lang$core$Json_Decode$null(fallback),
						_1: {ctor: '[]'}
					}
				});
		};
		var handleResult = function (input) {
			var _p1 = A2(_elm_lang$core$Json_Decode$decodeValue, pathDecoder, input);
			if (_p1.ctor === 'Ok') {
				var _p2 = A2(
					_elm_lang$core$Json_Decode$decodeValue,
					nullOr(valDecoder),
					_p1._0);
				if (_p2.ctor === 'Ok') {
					return _elm_lang$core$Json_Decode$succeed(_p2._0);
				} else {
					return _elm_lang$core$Json_Decode$fail(_p2._0);
				}
			} else {
				return _elm_lang$core$Json_Decode$succeed(fallback);
			}
		};
		return A2(_elm_lang$core$Json_Decode$andThen, handleResult, _elm_lang$core$Json_Decode$value);
	});
var _NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$optionalAt = F4(
	function (path, valDecoder, fallback, decoder) {
		return A2(
			_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$custom,
			A3(
				_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$optionalDecoder,
				A2(_elm_lang$core$Json_Decode$at, path, _elm_lang$core$Json_Decode$value),
				valDecoder,
				fallback),
			decoder);
	});
var _NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$optional = F4(
	function (key, valDecoder, fallback, decoder) {
		return A2(
			_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$custom,
			A3(
				_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$optionalDecoder,
				A2(_elm_lang$core$Json_Decode$field, key, _elm_lang$core$Json_Decode$value),
				valDecoder,
				fallback),
			decoder);
	});
var _NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$requiredAt = F3(
	function (path, valDecoder, decoder) {
		return A2(
			_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$custom,
			A2(_elm_lang$core$Json_Decode$at, path, valDecoder),
			decoder);
	});
var _NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required = F3(
	function (key, valDecoder, decoder) {
		return A2(
			_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$custom,
			A2(_elm_lang$core$Json_Decode$field, key, valDecoder),
			decoder);
	});

var _elm_lang$virtual_dom$VirtualDom_Debug$wrap;
var _elm_lang$virtual_dom$VirtualDom_Debug$wrapWithFlags;

var _elm_lang$virtual_dom$Native_VirtualDom = function() {

var STYLE_KEY = 'STYLE';
var EVENT_KEY = 'EVENT';
var ATTR_KEY = 'ATTR';
var ATTR_NS_KEY = 'ATTR_NS';

var localDoc = typeof document !== 'undefined' ? document : {};


////////////  VIRTUAL DOM NODES  ////////////


function text(string)
{
	return {
		type: 'text',
		text: string
	};
}


function node(tag)
{
	return F2(function(factList, kidList) {
		return nodeHelp(tag, factList, kidList);
	});
}


function nodeHelp(tag, factList, kidList)
{
	var organized = organizeFacts(factList);
	var namespace = organized.namespace;
	var facts = organized.facts;

	var children = [];
	var descendantsCount = 0;
	while (kidList.ctor !== '[]')
	{
		var kid = kidList._0;
		descendantsCount += (kid.descendantsCount || 0);
		children.push(kid);
		kidList = kidList._1;
	}
	descendantsCount += children.length;

	return {
		type: 'node',
		tag: tag,
		facts: facts,
		children: children,
		namespace: namespace,
		descendantsCount: descendantsCount
	};
}


function keyedNode(tag, factList, kidList)
{
	var organized = organizeFacts(factList);
	var namespace = organized.namespace;
	var facts = organized.facts;

	var children = [];
	var descendantsCount = 0;
	while (kidList.ctor !== '[]')
	{
		var kid = kidList._0;
		descendantsCount += (kid._1.descendantsCount || 0);
		children.push(kid);
		kidList = kidList._1;
	}
	descendantsCount += children.length;

	return {
		type: 'keyed-node',
		tag: tag,
		facts: facts,
		children: children,
		namespace: namespace,
		descendantsCount: descendantsCount
	};
}


function custom(factList, model, impl)
{
	var facts = organizeFacts(factList).facts;

	return {
		type: 'custom',
		facts: facts,
		model: model,
		impl: impl
	};
}


function map(tagger, node)
{
	return {
		type: 'tagger',
		tagger: tagger,
		node: node,
		descendantsCount: 1 + (node.descendantsCount || 0)
	};
}


function thunk(func, args, thunk)
{
	return {
		type: 'thunk',
		func: func,
		args: args,
		thunk: thunk,
		node: undefined
	};
}

function lazy(fn, a)
{
	return thunk(fn, [a], function() {
		return fn(a);
	});
}

function lazy2(fn, a, b)
{
	return thunk(fn, [a,b], function() {
		return A2(fn, a, b);
	});
}

function lazy3(fn, a, b, c)
{
	return thunk(fn, [a,b,c], function() {
		return A3(fn, a, b, c);
	});
}



// FACTS


function organizeFacts(factList)
{
	var namespace, facts = {};

	while (factList.ctor !== '[]')
	{
		var entry = factList._0;
		var key = entry.key;

		if (key === ATTR_KEY || key === ATTR_NS_KEY || key === EVENT_KEY)
		{
			var subFacts = facts[key] || {};
			subFacts[entry.realKey] = entry.value;
			facts[key] = subFacts;
		}
		else if (key === STYLE_KEY)
		{
			var styles = facts[key] || {};
			var styleList = entry.value;
			while (styleList.ctor !== '[]')
			{
				var style = styleList._0;
				styles[style._0] = style._1;
				styleList = styleList._1;
			}
			facts[key] = styles;
		}
		else if (key === 'namespace')
		{
			namespace = entry.value;
		}
		else if (key === 'className')
		{
			var classes = facts[key];
			facts[key] = typeof classes === 'undefined'
				? entry.value
				: classes + ' ' + entry.value;
		}
 		else
		{
			facts[key] = entry.value;
		}
		factList = factList._1;
	}

	return {
		facts: facts,
		namespace: namespace
	};
}



////////////  PROPERTIES AND ATTRIBUTES  ////////////


function style(value)
{
	return {
		key: STYLE_KEY,
		value: value
	};
}


function property(key, value)
{
	return {
		key: key,
		value: value
	};
}


function attribute(key, value)
{
	return {
		key: ATTR_KEY,
		realKey: key,
		value: value
	};
}


function attributeNS(namespace, key, value)
{
	return {
		key: ATTR_NS_KEY,
		realKey: key,
		value: {
			value: value,
			namespace: namespace
		}
	};
}


function on(name, options, decoder)
{
	return {
		key: EVENT_KEY,
		realKey: name,
		value: {
			options: options,
			decoder: decoder
		}
	};
}


function equalEvents(a, b)
{
	if (a.options !== b.options)
	{
		if (a.options.stopPropagation !== b.options.stopPropagation || a.options.preventDefault !== b.options.preventDefault)
		{
			return false;
		}
	}
	return _elm_lang$core$Native_Json.equality(a.decoder, b.decoder);
}


function mapProperty(func, property)
{
	if (property.key !== EVENT_KEY)
	{
		return property;
	}
	return on(
		property.realKey,
		property.value.options,
		A2(_elm_lang$core$Json_Decode$map, func, property.value.decoder)
	);
}


////////////  RENDER  ////////////


function render(vNode, eventNode)
{
	switch (vNode.type)
	{
		case 'thunk':
			if (!vNode.node)
			{
				vNode.node = vNode.thunk();
			}
			return render(vNode.node, eventNode);

		case 'tagger':
			var subNode = vNode.node;
			var tagger = vNode.tagger;

			while (subNode.type === 'tagger')
			{
				typeof tagger !== 'object'
					? tagger = [tagger, subNode.tagger]
					: tagger.push(subNode.tagger);

				subNode = subNode.node;
			}

			var subEventRoot = { tagger: tagger, parent: eventNode };
			var domNode = render(subNode, subEventRoot);
			domNode.elm_event_node_ref = subEventRoot;
			return domNode;

		case 'text':
			return localDoc.createTextNode(vNode.text);

		case 'node':
			var domNode = vNode.namespace
				? localDoc.createElementNS(vNode.namespace, vNode.tag)
				: localDoc.createElement(vNode.tag);

			applyFacts(domNode, eventNode, vNode.facts);

			var children = vNode.children;

			for (var i = 0; i < children.length; i++)
			{
				domNode.appendChild(render(children[i], eventNode));
			}

			return domNode;

		case 'keyed-node':
			var domNode = vNode.namespace
				? localDoc.createElementNS(vNode.namespace, vNode.tag)
				: localDoc.createElement(vNode.tag);

			applyFacts(domNode, eventNode, vNode.facts);

			var children = vNode.children;

			for (var i = 0; i < children.length; i++)
			{
				domNode.appendChild(render(children[i]._1, eventNode));
			}

			return domNode;

		case 'custom':
			var domNode = vNode.impl.render(vNode.model);
			applyFacts(domNode, eventNode, vNode.facts);
			return domNode;
	}
}



////////////  APPLY FACTS  ////////////


function applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		switch (key)
		{
			case STYLE_KEY:
				applyStyles(domNode, value);
				break;

			case EVENT_KEY:
				applyEvents(domNode, eventNode, value);
				break;

			case ATTR_KEY:
				applyAttrs(domNode, value);
				break;

			case ATTR_NS_KEY:
				applyAttrsNS(domNode, value);
				break;

			case 'value':
				if (domNode[key] !== value)
				{
					domNode[key] = value;
				}
				break;

			default:
				domNode[key] = value;
				break;
		}
	}
}

function applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}

function applyEvents(domNode, eventNode, events)
{
	var allHandlers = domNode.elm_handlers || {};

	for (var key in events)
	{
		var handler = allHandlers[key];
		var value = events[key];

		if (typeof value === 'undefined')
		{
			domNode.removeEventListener(key, handler);
			allHandlers[key] = undefined;
		}
		else if (typeof handler === 'undefined')
		{
			var handler = makeEventHandler(eventNode, value);
			domNode.addEventListener(key, handler);
			allHandlers[key] = handler;
		}
		else
		{
			handler.info = value;
		}
	}

	domNode.elm_handlers = allHandlers;
}

function makeEventHandler(eventNode, info)
{
	function eventHandler(event)
	{
		var info = eventHandler.info;

		var value = A2(_elm_lang$core$Native_Json.run, info.decoder, event);

		if (value.ctor === 'Ok')
		{
			var options = info.options;
			if (options.stopPropagation)
			{
				event.stopPropagation();
			}
			if (options.preventDefault)
			{
				event.preventDefault();
			}

			var message = value._0;

			var currentEventNode = eventNode;
			while (currentEventNode)
			{
				var tagger = currentEventNode.tagger;
				if (typeof tagger === 'function')
				{
					message = tagger(message);
				}
				else
				{
					for (var i = tagger.length; i--; )
					{
						message = tagger[i](message);
					}
				}
				currentEventNode = currentEventNode.parent;
			}
		}
	};

	eventHandler.info = info;

	return eventHandler;
}

function applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		if (typeof value === 'undefined')
		{
			domNode.removeAttribute(key);
		}
		else
		{
			domNode.setAttribute(key, value);
		}
	}
}

function applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.namespace;
		var value = pair.value;

		if (typeof value === 'undefined')
		{
			domNode.removeAttributeNS(namespace, key);
		}
		else
		{
			domNode.setAttributeNS(namespace, key, value);
		}
	}
}



////////////  DIFF  ////////////


function diff(a, b)
{
	var patches = [];
	diffHelp(a, b, patches, 0);
	return patches;
}


function makePatch(type, index, data)
{
	return {
		index: index,
		type: type,
		data: data,
		domNode: undefined,
		eventNode: undefined
	};
}


function diffHelp(a, b, patches, index)
{
	if (a === b)
	{
		return;
	}

	var aType = a.type;
	var bType = b.type;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (aType !== bType)
	{
		patches.push(makePatch('p-redraw', index, b));
		return;
	}

	// Now we know that both nodes are the same type.
	switch (bType)
	{
		case 'thunk':
			var aArgs = a.args;
			var bArgs = b.args;
			var i = aArgs.length;
			var same = a.func === b.func && i === bArgs.length;
			while (same && i--)
			{
				same = aArgs[i] === bArgs[i];
			}
			if (same)
			{
				b.node = a.node;
				return;
			}
			b.node = b.thunk();
			var subPatches = [];
			diffHelp(a.node, b.node, subPatches, 0);
			if (subPatches.length > 0)
			{
				patches.push(makePatch('p-thunk', index, subPatches));
			}
			return;

		case 'tagger':
			// gather nested taggers
			var aTaggers = a.tagger;
			var bTaggers = b.tagger;
			var nesting = false;

			var aSubNode = a.node;
			while (aSubNode.type === 'tagger')
			{
				nesting = true;

				typeof aTaggers !== 'object'
					? aTaggers = [aTaggers, aSubNode.tagger]
					: aTaggers.push(aSubNode.tagger);

				aSubNode = aSubNode.node;
			}

			var bSubNode = b.node;
			while (bSubNode.type === 'tagger')
			{
				nesting = true;

				typeof bTaggers !== 'object'
					? bTaggers = [bTaggers, bSubNode.tagger]
					: bTaggers.push(bSubNode.tagger);

				bSubNode = bSubNode.node;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && aTaggers.length !== bTaggers.length)
			{
				patches.push(makePatch('p-redraw', index, b));
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !pairwiseRefEqual(aTaggers, bTaggers) : aTaggers !== bTaggers)
			{
				patches.push(makePatch('p-tagger', index, bTaggers));
			}

			// diff everything below the taggers
			diffHelp(aSubNode, bSubNode, patches, index + 1);
			return;

		case 'text':
			if (a.text !== b.text)
			{
				patches.push(makePatch('p-text', index, b.text));
				return;
			}

			return;

		case 'node':
			// Bail if obvious indicators have changed. Implies more serious
			// structural changes such that it's not worth it to diff.
			if (a.tag !== b.tag || a.namespace !== b.namespace)
			{
				patches.push(makePatch('p-redraw', index, b));
				return;
			}

			var factsDiff = diffFacts(a.facts, b.facts);

			if (typeof factsDiff !== 'undefined')
			{
				patches.push(makePatch('p-facts', index, factsDiff));
			}

			diffChildren(a, b, patches, index);
			return;

		case 'keyed-node':
			// Bail if obvious indicators have changed. Implies more serious
			// structural changes such that it's not worth it to diff.
			if (a.tag !== b.tag || a.namespace !== b.namespace)
			{
				patches.push(makePatch('p-redraw', index, b));
				return;
			}

			var factsDiff = diffFacts(a.facts, b.facts);

			if (typeof factsDiff !== 'undefined')
			{
				patches.push(makePatch('p-facts', index, factsDiff));
			}

			diffKeyedChildren(a, b, patches, index);
			return;

		case 'custom':
			if (a.impl !== b.impl)
			{
				patches.push(makePatch('p-redraw', index, b));
				return;
			}

			var factsDiff = diffFacts(a.facts, b.facts);
			if (typeof factsDiff !== 'undefined')
			{
				patches.push(makePatch('p-facts', index, factsDiff));
			}

			var patch = b.impl.diff(a,b);
			if (patch)
			{
				patches.push(makePatch('p-custom', index, patch));
				return;
			}

			return;
	}
}


// assumes the incoming arrays are the same length
function pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function diffFacts(a, b, category)
{
	var diff;

	// look for changes and removals
	for (var aKey in a)
	{
		if (aKey === STYLE_KEY || aKey === EVENT_KEY || aKey === ATTR_KEY || aKey === ATTR_NS_KEY)
		{
			var subDiff = diffFacts(a[aKey], b[aKey] || {}, aKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[aKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(aKey in b))
		{
			diff = diff || {};
			diff[aKey] =
				(typeof category === 'undefined')
					? (typeof a[aKey] === 'string' ? '' : null)
					:
				(category === STYLE_KEY)
					? ''
					:
				(category === EVENT_KEY || category === ATTR_KEY)
					? undefined
					:
				{ namespace: a[aKey].namespace, value: undefined };

			continue;
		}

		var aValue = a[aKey];
		var bValue = b[aKey];

		// reference equal, so don't worry about it
		if (aValue === bValue && aKey !== 'value'
			|| category === EVENT_KEY && equalEvents(aValue, bValue))
		{
			continue;
		}

		diff = diff || {};
		diff[aKey] = bValue;
	}

	// add new stuff
	for (var bKey in b)
	{
		if (!(bKey in a))
		{
			diff = diff || {};
			diff[bKey] = b[bKey];
		}
	}

	return diff;
}


function diffChildren(aParent, bParent, patches, rootIndex)
{
	var aChildren = aParent.children;
	var bChildren = bParent.children;

	var aLen = aChildren.length;
	var bLen = bChildren.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (aLen > bLen)
	{
		patches.push(makePatch('p-remove-last', rootIndex, aLen - bLen));
	}
	else if (aLen < bLen)
	{
		patches.push(makePatch('p-append', rootIndex, bChildren.slice(aLen)));
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	var index = rootIndex;
	var minLen = aLen < bLen ? aLen : bLen;
	for (var i = 0; i < minLen; i++)
	{
		index++;
		var aChild = aChildren[i];
		diffHelp(aChild, bChildren[i], patches, index);
		index += aChild.descendantsCount || 0;
	}
}



////////////  KEYED DIFF  ////////////


function diffKeyedChildren(aParent, bParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var aChildren = aParent.children;
	var bChildren = bParent.children;
	var aLen = aChildren.length;
	var bLen = bChildren.length;
	var aIndex = 0;
	var bIndex = 0;

	var index = rootIndex;

	while (aIndex < aLen && bIndex < bLen)
	{
		var a = aChildren[aIndex];
		var b = bChildren[bIndex];

		var aKey = a._0;
		var bKey = b._0;
		var aNode = a._1;
		var bNode = b._1;

		// check if keys match

		if (aKey === bKey)
		{
			index++;
			diffHelp(aNode, bNode, localPatches, index);
			index += aNode.descendantsCount || 0;

			aIndex++;
			bIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var aLookAhead = aIndex + 1 < aLen;
		var bLookAhead = bIndex + 1 < bLen;

		if (aLookAhead)
		{
			var aNext = aChildren[aIndex + 1];
			var aNextKey = aNext._0;
			var aNextNode = aNext._1;
			var oldMatch = bKey === aNextKey;
		}

		if (bLookAhead)
		{
			var bNext = bChildren[bIndex + 1];
			var bNextKey = bNext._0;
			var bNextNode = bNext._1;
			var newMatch = aKey === bNextKey;
		}


		// swap a and b
		if (aLookAhead && bLookAhead && newMatch && oldMatch)
		{
			index++;
			diffHelp(aNode, bNextNode, localPatches, index);
			insertNode(changes, localPatches, aKey, bNode, bIndex, inserts);
			index += aNode.descendantsCount || 0;

			index++;
			removeNode(changes, localPatches, aKey, aNextNode, index);
			index += aNextNode.descendantsCount || 0;

			aIndex += 2;
			bIndex += 2;
			continue;
		}

		// insert b
		if (bLookAhead && newMatch)
		{
			index++;
			insertNode(changes, localPatches, bKey, bNode, bIndex, inserts);
			diffHelp(aNode, bNextNode, localPatches, index);
			index += aNode.descendantsCount || 0;

			aIndex += 1;
			bIndex += 2;
			continue;
		}

		// remove a
		if (aLookAhead && oldMatch)
		{
			index++;
			removeNode(changes, localPatches, aKey, aNode, index);
			index += aNode.descendantsCount || 0;

			index++;
			diffHelp(aNextNode, bNode, localPatches, index);
			index += aNextNode.descendantsCount || 0;

			aIndex += 2;
			bIndex += 1;
			continue;
		}

		// remove a, insert b
		if (aLookAhead && bLookAhead && aNextKey === bNextKey)
		{
			index++;
			removeNode(changes, localPatches, aKey, aNode, index);
			insertNode(changes, localPatches, bKey, bNode, bIndex, inserts);
			index += aNode.descendantsCount || 0;

			index++;
			diffHelp(aNextNode, bNextNode, localPatches, index);
			index += aNextNode.descendantsCount || 0;

			aIndex += 2;
			bIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (aIndex < aLen)
	{
		index++;
		var a = aChildren[aIndex];
		var aNode = a._1;
		removeNode(changes, localPatches, a._0, aNode, index);
		index += aNode.descendantsCount || 0;
		aIndex++;
	}

	var endInserts;
	while (bIndex < bLen)
	{
		endInserts = endInserts || [];
		var b = bChildren[bIndex];
		insertNode(changes, localPatches, b._0, b._1, undefined, endInserts);
		bIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || typeof endInserts !== 'undefined')
	{
		patches.push(makePatch('p-reorder', rootIndex, {
			patches: localPatches,
			inserts: inserts,
			endInserts: endInserts
		}));
	}
}



////////////  CHANGES FROM KEYED DIFF  ////////////


var POSTFIX = '_elmW6BL';


function insertNode(changes, localPatches, key, vnode, bIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (typeof entry === 'undefined')
	{
		entry = {
			tag: 'insert',
			vnode: vnode,
			index: bIndex,
			data: undefined
		};

		inserts.push({ index: bIndex, entry: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.tag === 'remove')
	{
		inserts.push({ index: bIndex, entry: entry });

		entry.tag = 'move';
		var subPatches = [];
		diffHelp(entry.vnode, vnode, subPatches, entry.index);
		entry.index = bIndex;
		entry.data.data = {
			patches: subPatches,
			entry: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	insertNode(changes, localPatches, key + POSTFIX, vnode, bIndex, inserts);
}


function removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (typeof entry === 'undefined')
	{
		var patch = makePatch('p-remove', index, undefined);
		localPatches.push(patch);

		changes[key] = {
			tag: 'remove',
			vnode: vnode,
			index: index,
			data: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.tag === 'insert')
	{
		entry.tag = 'move';
		var subPatches = [];
		diffHelp(vnode, entry.vnode, subPatches, index);

		var patch = makePatch('p-remove', index, {
			patches: subPatches,
			entry: entry
		});
		localPatches.push(patch);

		return;
	}

	// this key has already been removed or moved, a duplicate!
	removeNode(changes, localPatches, key + POSTFIX, vnode, index);
}



////////////  ADD DOM NODES  ////////////
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function addDomNodes(domNode, vNode, patches, eventNode)
{
	addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.descendantsCount, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.index;

	while (index === low)
	{
		var patchType = patch.type;

		if (patchType === 'p-thunk')
		{
			addDomNodes(domNode, vNode.node, patch.data, eventNode);
		}
		else if (patchType === 'p-reorder')
		{
			patch.domNode = domNode;
			patch.eventNode = eventNode;

			var subPatches = patch.data.patches;
			if (subPatches.length > 0)
			{
				addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 'p-remove')
		{
			patch.domNode = domNode;
			patch.eventNode = eventNode;

			var data = patch.data;
			if (typeof data !== 'undefined')
			{
				data.entry.data = domNode;
				var subPatches = data.patches;
				if (subPatches.length > 0)
				{
					addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.domNode = domNode;
			patch.eventNode = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.index) > high)
		{
			return i;
		}
	}

	switch (vNode.type)
	{
		case 'tagger':
			var subNode = vNode.node;

			while (subNode.type === "tagger")
			{
				subNode = subNode.node;
			}

			return addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);

		case 'node':
			var vChildren = vNode.children;
			var childNodes = domNode.childNodes;
			for (var j = 0; j < vChildren.length; j++)
			{
				low++;
				var vChild = vChildren[j];
				var nextLow = low + (vChild.descendantsCount || 0);
				if (low <= index && index <= nextLow)
				{
					i = addDomNodesHelp(childNodes[j], vChild, patches, i, low, nextLow, eventNode);
					if (!(patch = patches[i]) || (index = patch.index) > high)
					{
						return i;
					}
				}
				low = nextLow;
			}
			return i;

		case 'keyed-node':
			var vChildren = vNode.children;
			var childNodes = domNode.childNodes;
			for (var j = 0; j < vChildren.length; j++)
			{
				low++;
				var vChild = vChildren[j]._1;
				var nextLow = low + (vChild.descendantsCount || 0);
				if (low <= index && index <= nextLow)
				{
					i = addDomNodesHelp(childNodes[j], vChild, patches, i, low, nextLow, eventNode);
					if (!(patch = patches[i]) || (index = patch.index) > high)
					{
						return i;
					}
				}
				low = nextLow;
			}
			return i;

		case 'text':
		case 'thunk':
			throw new Error('should never traverse `text` or `thunk` nodes like this');
	}
}



////////////  APPLY PATCHES  ////////////


function applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return applyPatchesHelp(rootDomNode, patches);
}

function applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.domNode
		var newNode = applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function applyPatch(domNode, patch)
{
	switch (patch.type)
	{
		case 'p-redraw':
			return applyPatchRedraw(domNode, patch.data, patch.eventNode);

		case 'p-facts':
			applyFacts(domNode, patch.eventNode, patch.data);
			return domNode;

		case 'p-text':
			domNode.replaceData(0, domNode.length, patch.data);
			return domNode;

		case 'p-thunk':
			return applyPatchesHelp(domNode, patch.data);

		case 'p-tagger':
			if (typeof domNode.elm_event_node_ref !== 'undefined')
			{
				domNode.elm_event_node_ref.tagger = patch.data;
			}
			else
			{
				domNode.elm_event_node_ref = { tagger: patch.data, parent: patch.eventNode };
			}
			return domNode;

		case 'p-remove-last':
			var i = patch.data;
			while (i--)
			{
				domNode.removeChild(domNode.lastChild);
			}
			return domNode;

		case 'p-append':
			var newNodes = patch.data;
			for (var i = 0; i < newNodes.length; i++)
			{
				domNode.appendChild(render(newNodes[i], patch.eventNode));
			}
			return domNode;

		case 'p-remove':
			var data = patch.data;
			if (typeof data === 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.entry;
			if (typeof entry.index !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.data = applyPatchesHelp(domNode, data.patches);
			return domNode;

		case 'p-reorder':
			return applyPatchReorder(domNode, patch);

		case 'p-custom':
			var impl = patch.data;
			return impl.applyPatch(domNode, impl.data);

		default:
			throw new Error('Ran into an unknown patch!');
	}
}


function applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = render(vNode, eventNode);

	if (typeof newNode.elm_event_node_ref === 'undefined')
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function applyPatchReorder(domNode, patch)
{
	var data = patch.data;

	// remove end inserts
	var frag = applyPatchReorderEndInsertsHelp(data.endInserts, patch);

	// removals
	domNode = applyPatchesHelp(domNode, data.patches);

	// inserts
	var inserts = data.inserts;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.entry;
		var node = entry.tag === 'move'
			? entry.data
			: render(entry.vnode, patch.eventNode);
		domNode.insertBefore(node, domNode.childNodes[insert.index]);
	}

	// add end inserts
	if (typeof frag !== 'undefined')
	{
		domNode.appendChild(frag);
	}

	return domNode;
}


function applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (typeof endInserts === 'undefined')
	{
		return;
	}

	var frag = localDoc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.entry;
		frag.appendChild(entry.tag === 'move'
			? entry.data
			: render(entry.vnode, patch.eventNode)
		);
	}
	return frag;
}


// PROGRAMS

var program = makeProgram(checkNoFlags);
var programWithFlags = makeProgram(checkYesFlags);

function makeProgram(flagChecker)
{
	return F2(function(debugWrap, impl)
	{
		return function(flagDecoder)
		{
			return function(object, moduleName, debugMetadata)
			{
				var checker = flagChecker(flagDecoder, moduleName);
				if (typeof debugMetadata === 'undefined')
				{
					normalSetup(impl, object, moduleName, checker);
				}
				else
				{
					debugSetup(A2(debugWrap, debugMetadata, impl), object, moduleName, checker);
				}
			};
		};
	});
}

function staticProgram(vNode)
{
	var nothing = _elm_lang$core$Native_Utils.Tuple2(
		_elm_lang$core$Native_Utils.Tuple0,
		_elm_lang$core$Platform_Cmd$none
	);
	return A2(program, _elm_lang$virtual_dom$VirtualDom_Debug$wrap, {
		init: nothing,
		view: function() { return vNode; },
		update: F2(function() { return nothing; }),
		subscriptions: function() { return _elm_lang$core$Platform_Sub$none; }
	})();
}


// FLAG CHECKERS

function checkNoFlags(flagDecoder, moduleName)
{
	return function(init, flags, domNode)
	{
		if (typeof flags === 'undefined')
		{
			return init;
		}

		var errorMessage =
			'The `' + moduleName + '` module does not need flags.\n'
			+ 'Initialize it with no arguments and you should be all set!';

		crash(errorMessage, domNode);
	};
}

function checkYesFlags(flagDecoder, moduleName)
{
	return function(init, flags, domNode)
	{
		if (typeof flagDecoder === 'undefined')
		{
			var errorMessage =
				'Are you trying to sneak a Never value into Elm? Trickster!\n'
				+ 'It looks like ' + moduleName + '.main is defined with `programWithFlags` but has type `Program Never`.\n'
				+ 'Use `program` instead if you do not want flags.'

			crash(errorMessage, domNode);
		}

		var result = A2(_elm_lang$core$Native_Json.run, flagDecoder, flags);
		if (result.ctor === 'Ok')
		{
			return init(result._0);
		}

		var errorMessage =
			'Trying to initialize the `' + moduleName + '` module with an unexpected flag.\n'
			+ 'I tried to convert it to an Elm value, but ran into this problem:\n\n'
			+ result._0;

		crash(errorMessage, domNode);
	};
}

function crash(errorMessage, domNode)
{
	if (domNode)
	{
		domNode.innerHTML =
			'<div style="padding-left:1em;">'
			+ '<h2 style="font-weight:normal;"><b>Oops!</b> Something went wrong when starting your Elm program.</h2>'
			+ '<pre style="padding-left:1em;">' + errorMessage + '</pre>'
			+ '</div>';
	}

	throw new Error(errorMessage);
}


//  NORMAL SETUP

function normalSetup(impl, object, moduleName, flagChecker)
{
	object['embed'] = function embed(node, flags)
	{
		while (node.lastChild)
		{
			node.removeChild(node.lastChild);
		}

		return _elm_lang$core$Native_Platform.initialize(
			flagChecker(impl.init, flags, node),
			impl.update,
			impl.subscriptions,
			normalRenderer(node, impl.view)
		);
	};

	object['fullscreen'] = function fullscreen(flags)
	{
		return _elm_lang$core$Native_Platform.initialize(
			flagChecker(impl.init, flags, document.body),
			impl.update,
			impl.subscriptions,
			normalRenderer(document.body, impl.view)
		);
	};
}

function normalRenderer(parentNode, view)
{
	return function(tagger, initialModel)
	{
		var eventNode = { tagger: tagger, parent: undefined };
		var initialVirtualNode = view(initialModel);
		var domNode = render(initialVirtualNode, eventNode);
		parentNode.appendChild(domNode);
		return makeStepper(domNode, view, initialVirtualNode, eventNode);
	};
}


// STEPPER

var rAF =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { setTimeout(callback, 1000 / 60); };

function makeStepper(domNode, view, initialVirtualNode, eventNode)
{
	var state = 'NO_REQUEST';
	var currNode = initialVirtualNode;
	var nextModel;

	function updateIfNeeded()
	{
		switch (state)
		{
			case 'NO_REQUEST':
				throw new Error(
					'Unexpected draw callback.\n' +
					'Please report this to <https://github.com/elm-lang/virtual-dom/issues>.'
				);

			case 'PENDING_REQUEST':
				rAF(updateIfNeeded);
				state = 'EXTRA_REQUEST';

				var nextNode = view(nextModel);
				var patches = diff(currNode, nextNode);
				domNode = applyPatches(domNode, currNode, patches, eventNode);
				currNode = nextNode;

				return;

			case 'EXTRA_REQUEST':
				state = 'NO_REQUEST';
				return;
		}
	}

	return function stepper(model)
	{
		if (state === 'NO_REQUEST')
		{
			rAF(updateIfNeeded);
		}
		state = 'PENDING_REQUEST';
		nextModel = model;
	};
}


// DEBUG SETUP

function debugSetup(impl, object, moduleName, flagChecker)
{
	object['fullscreen'] = function fullscreen(flags)
	{
		var popoutRef = { doc: undefined };
		return _elm_lang$core$Native_Platform.initialize(
			flagChecker(impl.init, flags, document.body),
			impl.update(scrollTask(popoutRef)),
			impl.subscriptions,
			debugRenderer(moduleName, document.body, popoutRef, impl.view, impl.viewIn, impl.viewOut)
		);
	};

	object['embed'] = function fullscreen(node, flags)
	{
		var popoutRef = { doc: undefined };
		return _elm_lang$core$Native_Platform.initialize(
			flagChecker(impl.init, flags, node),
			impl.update(scrollTask(popoutRef)),
			impl.subscriptions,
			debugRenderer(moduleName, node, popoutRef, impl.view, impl.viewIn, impl.viewOut)
		);
	};
}

function scrollTask(popoutRef)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		var doc = popoutRef.doc;
		if (doc)
		{
			var msgs = doc.getElementsByClassName('debugger-sidebar-messages')[0];
			if (msgs)
			{
				msgs.scrollTop = msgs.scrollHeight;
			}
		}
		callback(_elm_lang$core$Native_Scheduler.succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}


function debugRenderer(moduleName, parentNode, popoutRef, view, viewIn, viewOut)
{
	return function(tagger, initialModel)
	{
		var appEventNode = { tagger: tagger, parent: undefined };
		var eventNode = { tagger: tagger, parent: undefined };

		// make normal stepper
		var appVirtualNode = view(initialModel);
		var appNode = render(appVirtualNode, appEventNode);
		parentNode.appendChild(appNode);
		var appStepper = makeStepper(appNode, view, appVirtualNode, appEventNode);

		// make overlay stepper
		var overVirtualNode = viewIn(initialModel)._1;
		var overNode = render(overVirtualNode, eventNode);
		parentNode.appendChild(overNode);
		var wrappedViewIn = wrapViewIn(appEventNode, overNode, viewIn);
		var overStepper = makeStepper(overNode, wrappedViewIn, overVirtualNode, eventNode);

		// make debugger stepper
		var debugStepper = makeDebugStepper(initialModel, viewOut, eventNode, parentNode, moduleName, popoutRef);

		return function stepper(model)
		{
			appStepper(model);
			overStepper(model);
			debugStepper(model);
		}
	};
}

function makeDebugStepper(initialModel, view, eventNode, parentNode, moduleName, popoutRef)
{
	var curr;
	var domNode;

	return function stepper(model)
	{
		if (!model.isDebuggerOpen)
		{
			return;
		}

		if (!popoutRef.doc)
		{
			curr = view(model);
			domNode = openDebugWindow(moduleName, popoutRef, curr, eventNode);
			return;
		}

		// switch to document of popout
		localDoc = popoutRef.doc;

		var next = view(model);
		var patches = diff(curr, next);
		domNode = applyPatches(domNode, curr, patches, eventNode);
		curr = next;

		// switch back to normal document
		localDoc = document;
	};
}

function openDebugWindow(moduleName, popoutRef, virtualNode, eventNode)
{
	var w = 900;
	var h = 360;
	var x = screen.width - w;
	var y = screen.height - h;
	var debugWindow = window.open('', '', 'width=' + w + ',height=' + h + ',left=' + x + ',top=' + y);

	// switch to window document
	localDoc = debugWindow.document;

	popoutRef.doc = localDoc;
	localDoc.title = 'Debugger - ' + moduleName;
	localDoc.body.style.margin = '0';
	localDoc.body.style.padding = '0';
	var domNode = render(virtualNode, eventNode);
	localDoc.body.appendChild(domNode);

	localDoc.addEventListener('keydown', function(event) {
		if (event.metaKey && event.which === 82)
		{
			window.location.reload();
		}
		if (event.which === 38)
		{
			eventNode.tagger({ ctor: 'Up' });
			event.preventDefault();
		}
		if (event.which === 40)
		{
			eventNode.tagger({ ctor: 'Down' });
			event.preventDefault();
		}
	});

	function close()
	{
		popoutRef.doc = undefined;
		debugWindow.close();
	}
	window.addEventListener('unload', close);
	debugWindow.addEventListener('unload', function() {
		popoutRef.doc = undefined;
		window.removeEventListener('unload', close);
		eventNode.tagger({ ctor: 'Close' });
	});

	// switch back to the normal document
	localDoc = document;

	return domNode;
}


// BLOCK EVENTS

function wrapViewIn(appEventNode, overlayNode, viewIn)
{
	var ignorer = makeIgnorer(overlayNode);
	var blocking = 'Normal';
	var overflow;

	var normalTagger = appEventNode.tagger;
	var blockTagger = function() {};

	return function(model)
	{
		var tuple = viewIn(model);
		var newBlocking = tuple._0.ctor;
		appEventNode.tagger = newBlocking === 'Normal' ? normalTagger : blockTagger;
		if (blocking !== newBlocking)
		{
			traverse('removeEventListener', ignorer, blocking);
			traverse('addEventListener', ignorer, newBlocking);

			if (blocking === 'Normal')
			{
				overflow = document.body.style.overflow;
				document.body.style.overflow = 'hidden';
			}

			if (newBlocking === 'Normal')
			{
				document.body.style.overflow = overflow;
			}

			blocking = newBlocking;
		}
		return tuple._1;
	}
}

function traverse(verbEventListener, ignorer, blocking)
{
	switch(blocking)
	{
		case 'Normal':
			return;

		case 'Pause':
			return traverseHelp(verbEventListener, ignorer, mostEvents);

		case 'Message':
			return traverseHelp(verbEventListener, ignorer, allEvents);
	}
}

function traverseHelp(verbEventListener, handler, eventNames)
{
	for (var i = 0; i < eventNames.length; i++)
	{
		document.body[verbEventListener](eventNames[i], handler, true);
	}
}

function makeIgnorer(overlayNode)
{
	return function(event)
	{
		if (event.type === 'keydown' && event.metaKey && event.which === 82)
		{
			return;
		}

		var isScroll = event.type === 'scroll' || event.type === 'wheel';

		var node = event.target;
		while (node !== null)
		{
			if (node.className === 'elm-overlay-message-details' && isScroll)
			{
				return;
			}

			if (node === overlayNode && !isScroll)
			{
				return;
			}
			node = node.parentNode;
		}

		event.stopPropagation();
		event.preventDefault();
	}
}

var mostEvents = [
	'click', 'dblclick', 'mousemove',
	'mouseup', 'mousedown', 'mouseenter', 'mouseleave',
	'touchstart', 'touchend', 'touchcancel', 'touchmove',
	'pointerdown', 'pointerup', 'pointerover', 'pointerout',
	'pointerenter', 'pointerleave', 'pointermove', 'pointercancel',
	'dragstart', 'drag', 'dragend', 'dragenter', 'dragover', 'dragleave', 'drop',
	'keyup', 'keydown', 'keypress',
	'input', 'change',
	'focus', 'blur'
];

var allEvents = mostEvents.concat('wheel', 'scroll');


return {
	node: node,
	text: text,
	custom: custom,
	map: F2(map),

	on: F3(on),
	style: style,
	property: F2(property),
	attribute: F2(attribute),
	attributeNS: F3(attributeNS),
	mapProperty: F2(mapProperty),

	lazy: F2(lazy),
	lazy2: F3(lazy2),
	lazy3: F4(lazy3),
	keyedNode: F3(keyedNode),

	program: program,
	programWithFlags: programWithFlags,
	staticProgram: staticProgram
};

}();

var _elm_lang$virtual_dom$VirtualDom$programWithFlags = function (impl) {
	return A2(_elm_lang$virtual_dom$Native_VirtualDom.programWithFlags, _elm_lang$virtual_dom$VirtualDom_Debug$wrapWithFlags, impl);
};
var _elm_lang$virtual_dom$VirtualDom$program = function (impl) {
	return A2(_elm_lang$virtual_dom$Native_VirtualDom.program, _elm_lang$virtual_dom$VirtualDom_Debug$wrap, impl);
};
var _elm_lang$virtual_dom$VirtualDom$keyedNode = _elm_lang$virtual_dom$Native_VirtualDom.keyedNode;
var _elm_lang$virtual_dom$VirtualDom$lazy3 = _elm_lang$virtual_dom$Native_VirtualDom.lazy3;
var _elm_lang$virtual_dom$VirtualDom$lazy2 = _elm_lang$virtual_dom$Native_VirtualDom.lazy2;
var _elm_lang$virtual_dom$VirtualDom$lazy = _elm_lang$virtual_dom$Native_VirtualDom.lazy;
var _elm_lang$virtual_dom$VirtualDom$defaultOptions = {stopPropagation: false, preventDefault: false};
var _elm_lang$virtual_dom$VirtualDom$onWithOptions = _elm_lang$virtual_dom$Native_VirtualDom.on;
var _elm_lang$virtual_dom$VirtualDom$on = F2(
	function (eventName, decoder) {
		return A3(_elm_lang$virtual_dom$VirtualDom$onWithOptions, eventName, _elm_lang$virtual_dom$VirtualDom$defaultOptions, decoder);
	});
var _elm_lang$virtual_dom$VirtualDom$style = _elm_lang$virtual_dom$Native_VirtualDom.style;
var _elm_lang$virtual_dom$VirtualDom$mapProperty = _elm_lang$virtual_dom$Native_VirtualDom.mapProperty;
var _elm_lang$virtual_dom$VirtualDom$attributeNS = _elm_lang$virtual_dom$Native_VirtualDom.attributeNS;
var _elm_lang$virtual_dom$VirtualDom$attribute = _elm_lang$virtual_dom$Native_VirtualDom.attribute;
var _elm_lang$virtual_dom$VirtualDom$property = _elm_lang$virtual_dom$Native_VirtualDom.property;
var _elm_lang$virtual_dom$VirtualDom$map = _elm_lang$virtual_dom$Native_VirtualDom.map;
var _elm_lang$virtual_dom$VirtualDom$text = _elm_lang$virtual_dom$Native_VirtualDom.text;
var _elm_lang$virtual_dom$VirtualDom$node = _elm_lang$virtual_dom$Native_VirtualDom.node;
var _elm_lang$virtual_dom$VirtualDom$Options = F2(
	function (a, b) {
		return {stopPropagation: a, preventDefault: b};
	});
var _elm_lang$virtual_dom$VirtualDom$Node = {ctor: 'Node'};
var _elm_lang$virtual_dom$VirtualDom$Property = {ctor: 'Property'};

var _elm_lang$html$Html$programWithFlags = _elm_lang$virtual_dom$VirtualDom$programWithFlags;
var _elm_lang$html$Html$program = _elm_lang$virtual_dom$VirtualDom$program;
var _elm_lang$html$Html$beginnerProgram = function (_p0) {
	var _p1 = _p0;
	return _elm_lang$html$Html$program(
		{
			init: A2(
				_elm_lang$core$Platform_Cmd_ops['!'],
				_p1.model,
				{ctor: '[]'}),
			update: F2(
				function (msg, model) {
					return A2(
						_elm_lang$core$Platform_Cmd_ops['!'],
						A2(_p1.update, msg, model),
						{ctor: '[]'});
				}),
			view: _p1.view,
			subscriptions: function (_p2) {
				return _elm_lang$core$Platform_Sub$none;
			}
		});
};
var _elm_lang$html$Html$map = _elm_lang$virtual_dom$VirtualDom$map;
var _elm_lang$html$Html$text = _elm_lang$virtual_dom$VirtualDom$text;
var _elm_lang$html$Html$node = _elm_lang$virtual_dom$VirtualDom$node;
var _elm_lang$html$Html$body = _elm_lang$html$Html$node('body');
var _elm_lang$html$Html$section = _elm_lang$html$Html$node('section');
var _elm_lang$html$Html$nav = _elm_lang$html$Html$node('nav');
var _elm_lang$html$Html$article = _elm_lang$html$Html$node('article');
var _elm_lang$html$Html$aside = _elm_lang$html$Html$node('aside');
var _elm_lang$html$Html$h1 = _elm_lang$html$Html$node('h1');
var _elm_lang$html$Html$h2 = _elm_lang$html$Html$node('h2');
var _elm_lang$html$Html$h3 = _elm_lang$html$Html$node('h3');
var _elm_lang$html$Html$h4 = _elm_lang$html$Html$node('h4');
var _elm_lang$html$Html$h5 = _elm_lang$html$Html$node('h5');
var _elm_lang$html$Html$h6 = _elm_lang$html$Html$node('h6');
var _elm_lang$html$Html$header = _elm_lang$html$Html$node('header');
var _elm_lang$html$Html$footer = _elm_lang$html$Html$node('footer');
var _elm_lang$html$Html$address = _elm_lang$html$Html$node('address');
var _elm_lang$html$Html$main_ = _elm_lang$html$Html$node('main');
var _elm_lang$html$Html$p = _elm_lang$html$Html$node('p');
var _elm_lang$html$Html$hr = _elm_lang$html$Html$node('hr');
var _elm_lang$html$Html$pre = _elm_lang$html$Html$node('pre');
var _elm_lang$html$Html$blockquote = _elm_lang$html$Html$node('blockquote');
var _elm_lang$html$Html$ol = _elm_lang$html$Html$node('ol');
var _elm_lang$html$Html$ul = _elm_lang$html$Html$node('ul');
var _elm_lang$html$Html$li = _elm_lang$html$Html$node('li');
var _elm_lang$html$Html$dl = _elm_lang$html$Html$node('dl');
var _elm_lang$html$Html$dt = _elm_lang$html$Html$node('dt');
var _elm_lang$html$Html$dd = _elm_lang$html$Html$node('dd');
var _elm_lang$html$Html$figure = _elm_lang$html$Html$node('figure');
var _elm_lang$html$Html$figcaption = _elm_lang$html$Html$node('figcaption');
var _elm_lang$html$Html$div = _elm_lang$html$Html$node('div');
var _elm_lang$html$Html$a = _elm_lang$html$Html$node('a');
var _elm_lang$html$Html$em = _elm_lang$html$Html$node('em');
var _elm_lang$html$Html$strong = _elm_lang$html$Html$node('strong');
var _elm_lang$html$Html$small = _elm_lang$html$Html$node('small');
var _elm_lang$html$Html$s = _elm_lang$html$Html$node('s');
var _elm_lang$html$Html$cite = _elm_lang$html$Html$node('cite');
var _elm_lang$html$Html$q = _elm_lang$html$Html$node('q');
var _elm_lang$html$Html$dfn = _elm_lang$html$Html$node('dfn');
var _elm_lang$html$Html$abbr = _elm_lang$html$Html$node('abbr');
var _elm_lang$html$Html$time = _elm_lang$html$Html$node('time');
var _elm_lang$html$Html$code = _elm_lang$html$Html$node('code');
var _elm_lang$html$Html$var = _elm_lang$html$Html$node('var');
var _elm_lang$html$Html$samp = _elm_lang$html$Html$node('samp');
var _elm_lang$html$Html$kbd = _elm_lang$html$Html$node('kbd');
var _elm_lang$html$Html$sub = _elm_lang$html$Html$node('sub');
var _elm_lang$html$Html$sup = _elm_lang$html$Html$node('sup');
var _elm_lang$html$Html$i = _elm_lang$html$Html$node('i');
var _elm_lang$html$Html$b = _elm_lang$html$Html$node('b');
var _elm_lang$html$Html$u = _elm_lang$html$Html$node('u');
var _elm_lang$html$Html$mark = _elm_lang$html$Html$node('mark');
var _elm_lang$html$Html$ruby = _elm_lang$html$Html$node('ruby');
var _elm_lang$html$Html$rt = _elm_lang$html$Html$node('rt');
var _elm_lang$html$Html$rp = _elm_lang$html$Html$node('rp');
var _elm_lang$html$Html$bdi = _elm_lang$html$Html$node('bdi');
var _elm_lang$html$Html$bdo = _elm_lang$html$Html$node('bdo');
var _elm_lang$html$Html$span = _elm_lang$html$Html$node('span');
var _elm_lang$html$Html$br = _elm_lang$html$Html$node('br');
var _elm_lang$html$Html$wbr = _elm_lang$html$Html$node('wbr');
var _elm_lang$html$Html$ins = _elm_lang$html$Html$node('ins');
var _elm_lang$html$Html$del = _elm_lang$html$Html$node('del');
var _elm_lang$html$Html$img = _elm_lang$html$Html$node('img');
var _elm_lang$html$Html$iframe = _elm_lang$html$Html$node('iframe');
var _elm_lang$html$Html$embed = _elm_lang$html$Html$node('embed');
var _elm_lang$html$Html$object = _elm_lang$html$Html$node('object');
var _elm_lang$html$Html$param = _elm_lang$html$Html$node('param');
var _elm_lang$html$Html$video = _elm_lang$html$Html$node('video');
var _elm_lang$html$Html$audio = _elm_lang$html$Html$node('audio');
var _elm_lang$html$Html$source = _elm_lang$html$Html$node('source');
var _elm_lang$html$Html$track = _elm_lang$html$Html$node('track');
var _elm_lang$html$Html$canvas = _elm_lang$html$Html$node('canvas');
var _elm_lang$html$Html$math = _elm_lang$html$Html$node('math');
var _elm_lang$html$Html$table = _elm_lang$html$Html$node('table');
var _elm_lang$html$Html$caption = _elm_lang$html$Html$node('caption');
var _elm_lang$html$Html$colgroup = _elm_lang$html$Html$node('colgroup');
var _elm_lang$html$Html$col = _elm_lang$html$Html$node('col');
var _elm_lang$html$Html$tbody = _elm_lang$html$Html$node('tbody');
var _elm_lang$html$Html$thead = _elm_lang$html$Html$node('thead');
var _elm_lang$html$Html$tfoot = _elm_lang$html$Html$node('tfoot');
var _elm_lang$html$Html$tr = _elm_lang$html$Html$node('tr');
var _elm_lang$html$Html$td = _elm_lang$html$Html$node('td');
var _elm_lang$html$Html$th = _elm_lang$html$Html$node('th');
var _elm_lang$html$Html$form = _elm_lang$html$Html$node('form');
var _elm_lang$html$Html$fieldset = _elm_lang$html$Html$node('fieldset');
var _elm_lang$html$Html$legend = _elm_lang$html$Html$node('legend');
var _elm_lang$html$Html$label = _elm_lang$html$Html$node('label');
var _elm_lang$html$Html$input = _elm_lang$html$Html$node('input');
var _elm_lang$html$Html$button = _elm_lang$html$Html$node('button');
var _elm_lang$html$Html$select = _elm_lang$html$Html$node('select');
var _elm_lang$html$Html$datalist = _elm_lang$html$Html$node('datalist');
var _elm_lang$html$Html$optgroup = _elm_lang$html$Html$node('optgroup');
var _elm_lang$html$Html$option = _elm_lang$html$Html$node('option');
var _elm_lang$html$Html$textarea = _elm_lang$html$Html$node('textarea');
var _elm_lang$html$Html$keygen = _elm_lang$html$Html$node('keygen');
var _elm_lang$html$Html$output = _elm_lang$html$Html$node('output');
var _elm_lang$html$Html$progress = _elm_lang$html$Html$node('progress');
var _elm_lang$html$Html$meter = _elm_lang$html$Html$node('meter');
var _elm_lang$html$Html$details = _elm_lang$html$Html$node('details');
var _elm_lang$html$Html$summary = _elm_lang$html$Html$node('summary');
var _elm_lang$html$Html$menuitem = _elm_lang$html$Html$node('menuitem');
var _elm_lang$html$Html$menu = _elm_lang$html$Html$node('menu');

var _elm_lang$html$Html_Attributes$map = _elm_lang$virtual_dom$VirtualDom$mapProperty;
var _elm_lang$html$Html_Attributes$attribute = _elm_lang$virtual_dom$VirtualDom$attribute;
var _elm_lang$html$Html_Attributes$contextmenu = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'contextmenu', value);
};
var _elm_lang$html$Html_Attributes$draggable = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'draggable', value);
};
var _elm_lang$html$Html_Attributes$itemprop = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'itemprop', value);
};
var _elm_lang$html$Html_Attributes$tabindex = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'tabIndex',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$charset = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'charset', value);
};
var _elm_lang$html$Html_Attributes$height = function (value) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'height',
		_elm_lang$core$Basics$toString(value));
};
var _elm_lang$html$Html_Attributes$width = function (value) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'width',
		_elm_lang$core$Basics$toString(value));
};
var _elm_lang$html$Html_Attributes$formaction = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'formAction', value);
};
var _elm_lang$html$Html_Attributes$list = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'list', value);
};
var _elm_lang$html$Html_Attributes$minlength = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'minLength',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$maxlength = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'maxlength',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$size = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'size',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$form = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'form', value);
};
var _elm_lang$html$Html_Attributes$cols = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'cols',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$rows = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'rows',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$challenge = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'challenge', value);
};
var _elm_lang$html$Html_Attributes$media = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'media', value);
};
var _elm_lang$html$Html_Attributes$rel = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'rel', value);
};
var _elm_lang$html$Html_Attributes$datetime = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'datetime', value);
};
var _elm_lang$html$Html_Attributes$pubdate = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'pubdate', value);
};
var _elm_lang$html$Html_Attributes$colspan = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'colspan',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$rowspan = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'rowspan',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$manifest = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'manifest', value);
};
var _elm_lang$html$Html_Attributes$property = _elm_lang$virtual_dom$VirtualDom$property;
var _elm_lang$html$Html_Attributes$stringProperty = F2(
	function (name, string) {
		return A2(
			_elm_lang$html$Html_Attributes$property,
			name,
			_elm_lang$core$Json_Encode$string(string));
	});
var _elm_lang$html$Html_Attributes$class = function (name) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'className', name);
};
var _elm_lang$html$Html_Attributes$id = function (name) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'id', name);
};
var _elm_lang$html$Html_Attributes$title = function (name) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'title', name);
};
var _elm_lang$html$Html_Attributes$accesskey = function ($char) {
	return A2(
		_elm_lang$html$Html_Attributes$stringProperty,
		'accessKey',
		_elm_lang$core$String$fromChar($char));
};
var _elm_lang$html$Html_Attributes$dir = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'dir', value);
};
var _elm_lang$html$Html_Attributes$dropzone = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'dropzone', value);
};
var _elm_lang$html$Html_Attributes$lang = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'lang', value);
};
var _elm_lang$html$Html_Attributes$content = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'content', value);
};
var _elm_lang$html$Html_Attributes$httpEquiv = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'httpEquiv', value);
};
var _elm_lang$html$Html_Attributes$language = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'language', value);
};
var _elm_lang$html$Html_Attributes$src = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'src', value);
};
var _elm_lang$html$Html_Attributes$alt = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'alt', value);
};
var _elm_lang$html$Html_Attributes$preload = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'preload', value);
};
var _elm_lang$html$Html_Attributes$poster = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'poster', value);
};
var _elm_lang$html$Html_Attributes$kind = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'kind', value);
};
var _elm_lang$html$Html_Attributes$srclang = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'srclang', value);
};
var _elm_lang$html$Html_Attributes$sandbox = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'sandbox', value);
};
var _elm_lang$html$Html_Attributes$srcdoc = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'srcdoc', value);
};
var _elm_lang$html$Html_Attributes$type_ = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'type', value);
};
var _elm_lang$html$Html_Attributes$value = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'value', value);
};
var _elm_lang$html$Html_Attributes$defaultValue = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'defaultValue', value);
};
var _elm_lang$html$Html_Attributes$placeholder = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'placeholder', value);
};
var _elm_lang$html$Html_Attributes$accept = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'accept', value);
};
var _elm_lang$html$Html_Attributes$acceptCharset = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'acceptCharset', value);
};
var _elm_lang$html$Html_Attributes$action = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'action', value);
};
var _elm_lang$html$Html_Attributes$autocomplete = function (bool) {
	return A2(
		_elm_lang$html$Html_Attributes$stringProperty,
		'autocomplete',
		bool ? 'on' : 'off');
};
var _elm_lang$html$Html_Attributes$enctype = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'enctype', value);
};
var _elm_lang$html$Html_Attributes$method = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'method', value);
};
var _elm_lang$html$Html_Attributes$name = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'name', value);
};
var _elm_lang$html$Html_Attributes$pattern = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'pattern', value);
};
var _elm_lang$html$Html_Attributes$for = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'htmlFor', value);
};
var _elm_lang$html$Html_Attributes$max = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'max', value);
};
var _elm_lang$html$Html_Attributes$min = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'min', value);
};
var _elm_lang$html$Html_Attributes$step = function (n) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'step', n);
};
var _elm_lang$html$Html_Attributes$wrap = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'wrap', value);
};
var _elm_lang$html$Html_Attributes$usemap = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'useMap', value);
};
var _elm_lang$html$Html_Attributes$shape = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'shape', value);
};
var _elm_lang$html$Html_Attributes$coords = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'coords', value);
};
var _elm_lang$html$Html_Attributes$keytype = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'keytype', value);
};
var _elm_lang$html$Html_Attributes$align = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'align', value);
};
var _elm_lang$html$Html_Attributes$cite = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'cite', value);
};
var _elm_lang$html$Html_Attributes$href = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'href', value);
};
var _elm_lang$html$Html_Attributes$target = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'target', value);
};
var _elm_lang$html$Html_Attributes$downloadAs = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'download', value);
};
var _elm_lang$html$Html_Attributes$hreflang = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'hreflang', value);
};
var _elm_lang$html$Html_Attributes$ping = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'ping', value);
};
var _elm_lang$html$Html_Attributes$start = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$stringProperty,
		'start',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$headers = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'headers', value);
};
var _elm_lang$html$Html_Attributes$scope = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'scope', value);
};
var _elm_lang$html$Html_Attributes$boolProperty = F2(
	function (name, bool) {
		return A2(
			_elm_lang$html$Html_Attributes$property,
			name,
			_elm_lang$core$Json_Encode$bool(bool));
	});
var _elm_lang$html$Html_Attributes$hidden = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'hidden', bool);
};
var _elm_lang$html$Html_Attributes$contenteditable = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'contentEditable', bool);
};
var _elm_lang$html$Html_Attributes$spellcheck = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'spellcheck', bool);
};
var _elm_lang$html$Html_Attributes$async = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'async', bool);
};
var _elm_lang$html$Html_Attributes$defer = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'defer', bool);
};
var _elm_lang$html$Html_Attributes$scoped = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'scoped', bool);
};
var _elm_lang$html$Html_Attributes$autoplay = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'autoplay', bool);
};
var _elm_lang$html$Html_Attributes$controls = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'controls', bool);
};
var _elm_lang$html$Html_Attributes$loop = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'loop', bool);
};
var _elm_lang$html$Html_Attributes$default = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'default', bool);
};
var _elm_lang$html$Html_Attributes$seamless = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'seamless', bool);
};
var _elm_lang$html$Html_Attributes$checked = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'checked', bool);
};
var _elm_lang$html$Html_Attributes$selected = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'selected', bool);
};
var _elm_lang$html$Html_Attributes$autofocus = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'autofocus', bool);
};
var _elm_lang$html$Html_Attributes$disabled = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'disabled', bool);
};
var _elm_lang$html$Html_Attributes$multiple = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'multiple', bool);
};
var _elm_lang$html$Html_Attributes$novalidate = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'noValidate', bool);
};
var _elm_lang$html$Html_Attributes$readonly = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'readOnly', bool);
};
var _elm_lang$html$Html_Attributes$required = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'required', bool);
};
var _elm_lang$html$Html_Attributes$ismap = function (value) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'isMap', value);
};
var _elm_lang$html$Html_Attributes$download = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'download', bool);
};
var _elm_lang$html$Html_Attributes$reversed = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'reversed', bool);
};
var _elm_lang$html$Html_Attributes$classList = function (list) {
	return _elm_lang$html$Html_Attributes$class(
		A2(
			_elm_lang$core$String$join,
			' ',
			A2(
				_elm_lang$core$List$map,
				_elm_lang$core$Tuple$first,
				A2(_elm_lang$core$List$filter, _elm_lang$core$Tuple$second, list))));
};
var _elm_lang$html$Html_Attributes$style = _elm_lang$virtual_dom$VirtualDom$style;

var _elm_lang$html$Html_Events$keyCode = A2(_elm_lang$core$Json_Decode$field, 'keyCode', _elm_lang$core$Json_Decode$int);
var _elm_lang$html$Html_Events$targetChecked = A2(
	_elm_lang$core$Json_Decode$at,
	{
		ctor: '::',
		_0: 'target',
		_1: {
			ctor: '::',
			_0: 'checked',
			_1: {ctor: '[]'}
		}
	},
	_elm_lang$core$Json_Decode$bool);
var _elm_lang$html$Html_Events$targetValue = A2(
	_elm_lang$core$Json_Decode$at,
	{
		ctor: '::',
		_0: 'target',
		_1: {
			ctor: '::',
			_0: 'value',
			_1: {ctor: '[]'}
		}
	},
	_elm_lang$core$Json_Decode$string);
var _elm_lang$html$Html_Events$defaultOptions = _elm_lang$virtual_dom$VirtualDom$defaultOptions;
var _elm_lang$html$Html_Events$onWithOptions = _elm_lang$virtual_dom$VirtualDom$onWithOptions;
var _elm_lang$html$Html_Events$on = _elm_lang$virtual_dom$VirtualDom$on;
var _elm_lang$html$Html_Events$onFocus = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'focus',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onBlur = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'blur',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onSubmitOptions = _elm_lang$core$Native_Utils.update(
	_elm_lang$html$Html_Events$defaultOptions,
	{preventDefault: true});
var _elm_lang$html$Html_Events$onSubmit = function (msg) {
	return A3(
		_elm_lang$html$Html_Events$onWithOptions,
		'submit',
		_elm_lang$html$Html_Events$onSubmitOptions,
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onCheck = function (tagger) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'change',
		A2(_elm_lang$core$Json_Decode$map, tagger, _elm_lang$html$Html_Events$targetChecked));
};
var _elm_lang$html$Html_Events$onInput = function (tagger) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'input',
		A2(_elm_lang$core$Json_Decode$map, tagger, _elm_lang$html$Html_Events$targetValue));
};
var _elm_lang$html$Html_Events$onMouseOut = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseout',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseOver = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseover',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseLeave = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseleave',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseEnter = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseenter',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseUp = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseup',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseDown = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mousedown',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onDoubleClick = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'dblclick',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onClick = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'click',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$Options = F2(
	function (a, b) {
		return {stopPropagation: a, preventDefault: b};
	});

var _debois$elm_dom$DOM$className = A2(
	_elm_lang$core$Json_Decode$at,
	{
		ctor: '::',
		_0: 'className',
		_1: {ctor: '[]'}
	},
	_elm_lang$core$Json_Decode$string);
var _debois$elm_dom$DOM$scrollTop = A2(_elm_lang$core$Json_Decode$field, 'scrollTop', _elm_lang$core$Json_Decode$float);
var _debois$elm_dom$DOM$scrollLeft = A2(_elm_lang$core$Json_Decode$field, 'scrollLeft', _elm_lang$core$Json_Decode$float);
var _debois$elm_dom$DOM$offsetTop = A2(_elm_lang$core$Json_Decode$field, 'offsetTop', _elm_lang$core$Json_Decode$float);
var _debois$elm_dom$DOM$offsetLeft = A2(_elm_lang$core$Json_Decode$field, 'offsetLeft', _elm_lang$core$Json_Decode$float);
var _debois$elm_dom$DOM$offsetHeight = A2(_elm_lang$core$Json_Decode$field, 'offsetHeight', _elm_lang$core$Json_Decode$float);
var _debois$elm_dom$DOM$offsetWidth = A2(_elm_lang$core$Json_Decode$field, 'offsetWidth', _elm_lang$core$Json_Decode$float);
var _debois$elm_dom$DOM$childNodes = function (decoder) {
	var loop = F2(
		function (idx, xs) {
			return A2(
				_elm_lang$core$Json_Decode$andThen,
				function (_p0) {
					return A2(
						_elm_lang$core$Maybe$withDefault,
						_elm_lang$core$Json_Decode$succeed(xs),
						A2(
							_elm_lang$core$Maybe$map,
							function (x) {
								return A2(
									loop,
									idx + 1,
									{ctor: '::', _0: x, _1: xs});
							},
							_p0));
				},
				_elm_lang$core$Json_Decode$maybe(
					A2(
						_elm_lang$core$Json_Decode$field,
						_elm_lang$core$Basics$toString(idx),
						decoder)));
		});
	return A2(
		_elm_lang$core$Json_Decode$map,
		_elm_lang$core$List$reverse,
		A2(
			_elm_lang$core$Json_Decode$field,
			'childNodes',
			A2(
				loop,
				0,
				{ctor: '[]'})));
};
var _debois$elm_dom$DOM$childNode = function (idx) {
	return _elm_lang$core$Json_Decode$at(
		{
			ctor: '::',
			_0: 'childNodes',
			_1: {
				ctor: '::',
				_0: _elm_lang$core$Basics$toString(idx),
				_1: {ctor: '[]'}
			}
		});
};
var _debois$elm_dom$DOM$parentElement = function (decoder) {
	return A2(_elm_lang$core$Json_Decode$field, 'parentElement', decoder);
};
var _debois$elm_dom$DOM$previousSibling = function (decoder) {
	return A2(_elm_lang$core$Json_Decode$field, 'previousSibling', decoder);
};
var _debois$elm_dom$DOM$nextSibling = function (decoder) {
	return A2(_elm_lang$core$Json_Decode$field, 'nextSibling', decoder);
};
var _debois$elm_dom$DOM$offsetParent = F2(
	function (x, decoder) {
		return _elm_lang$core$Json_Decode$oneOf(
			{
				ctor: '::',
				_0: A2(
					_elm_lang$core$Json_Decode$field,
					'offsetParent',
					_elm_lang$core$Json_Decode$null(x)),
				_1: {
					ctor: '::',
					_0: A2(_elm_lang$core$Json_Decode$field, 'offsetParent', decoder),
					_1: {ctor: '[]'}
				}
			});
	});
var _debois$elm_dom$DOM$position = F2(
	function (x, y) {
		return A2(
			_elm_lang$core$Json_Decode$andThen,
			function (_p1) {
				var _p2 = _p1;
				var _p4 = _p2._1;
				var _p3 = _p2._0;
				return A2(
					_debois$elm_dom$DOM$offsetParent,
					{ctor: '_Tuple2', _0: _p3, _1: _p4},
					A2(_debois$elm_dom$DOM$position, _p3, _p4));
			},
			A5(
				_elm_lang$core$Json_Decode$map4,
				F4(
					function (scrollLeft, scrollTop, offsetLeft, offsetTop) {
						return {ctor: '_Tuple2', _0: (x + offsetLeft) - scrollLeft, _1: (y + offsetTop) - scrollTop};
					}),
				_debois$elm_dom$DOM$scrollLeft,
				_debois$elm_dom$DOM$scrollTop,
				_debois$elm_dom$DOM$offsetLeft,
				_debois$elm_dom$DOM$offsetTop));
	});
var _debois$elm_dom$DOM$boundingClientRect = A4(
	_elm_lang$core$Json_Decode$map3,
	F3(
		function (_p5, width, height) {
			var _p6 = _p5;
			return {top: _p6._1, left: _p6._0, width: width, height: height};
		}),
	A2(_debois$elm_dom$DOM$position, 0, 0),
	_debois$elm_dom$DOM$offsetWidth,
	_debois$elm_dom$DOM$offsetHeight);
var _debois$elm_dom$DOM$target = function (decoder) {
	return A2(_elm_lang$core$Json_Decode$field, 'target', decoder);
};
var _debois$elm_dom$DOM$Rectangle = F4(
	function (a, b, c, d) {
		return {top: a, left: b, width: c, height: d};
	});

var _elm_lang$dom$Native_Dom = function() {

var fakeNode = {
	addEventListener: function() {},
	removeEventListener: function() {}
};

var onDocument = on(typeof document !== 'undefined' ? document : fakeNode);
var onWindow = on(typeof window !== 'undefined' ? window : fakeNode);

function on(node)
{
	return function(eventName, decoder, toTask)
	{
		return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback) {

			function performTask(event)
			{
				var result = A2(_elm_lang$core$Json_Decode$decodeValue, decoder, event);
				if (result.ctor === 'Ok')
				{
					_elm_lang$core$Native_Scheduler.rawSpawn(toTask(result._0));
				}
			}

			node.addEventListener(eventName, performTask);

			return function()
			{
				node.removeEventListener(eventName, performTask);
			};
		});
	};
}

var rAF = typeof requestAnimationFrame !== 'undefined'
	? requestAnimationFrame
	: function(callback) { callback(); };

function withNode(id, doStuff)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		rAF(function()
		{
			var node = document.getElementById(id);
			if (node === null)
			{
				callback(_elm_lang$core$Native_Scheduler.fail({ ctor: 'NotFound', _0: id }));
				return;
			}
			callback(_elm_lang$core$Native_Scheduler.succeed(doStuff(node)));
		});
	});
}


// FOCUS

function focus(id)
{
	return withNode(id, function(node) {
		node.focus();
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}

function blur(id)
{
	return withNode(id, function(node) {
		node.blur();
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}


// SCROLLING

function getScrollTop(id)
{
	return withNode(id, function(node) {
		return node.scrollTop;
	});
}

function setScrollTop(id, desiredScrollTop)
{
	return withNode(id, function(node) {
		node.scrollTop = desiredScrollTop;
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}

function toBottom(id)
{
	return withNode(id, function(node) {
		node.scrollTop = node.scrollHeight;
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}

function getScrollLeft(id)
{
	return withNode(id, function(node) {
		return node.scrollLeft;
	});
}

function setScrollLeft(id, desiredScrollLeft)
{
	return withNode(id, function(node) {
		node.scrollLeft = desiredScrollLeft;
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}

function toRight(id)
{
	return withNode(id, function(node) {
		node.scrollLeft = node.scrollWidth;
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}


// SIZE

function width(options, id)
{
	return withNode(id, function(node) {
		switch (options.ctor)
		{
			case 'Content':
				return node.scrollWidth;
			case 'VisibleContent':
				return node.clientWidth;
			case 'VisibleContentWithBorders':
				return node.offsetWidth;
			case 'VisibleContentWithBordersAndMargins':
				var rect = node.getBoundingClientRect();
				return rect.right - rect.left;
		}
	});
}

function height(options, id)
{
	return withNode(id, function(node) {
		switch (options.ctor)
		{
			case 'Content':
				return node.scrollHeight;
			case 'VisibleContent':
				return node.clientHeight;
			case 'VisibleContentWithBorders':
				return node.offsetHeight;
			case 'VisibleContentWithBordersAndMargins':
				var rect = node.getBoundingClientRect();
				return rect.bottom - rect.top;
		}
	});
}

return {
	onDocument: F3(onDocument),
	onWindow: F3(onWindow),

	focus: focus,
	blur: blur,

	getScrollTop: getScrollTop,
	setScrollTop: F2(setScrollTop),
	getScrollLeft: getScrollLeft,
	setScrollLeft: F2(setScrollLeft),
	toBottom: toBottom,
	toRight: toRight,

	height: F2(height),
	width: F2(width)
};

}();

var _elm_lang$dom$Dom_LowLevel$onWindow = _elm_lang$dom$Native_Dom.onWindow;
var _elm_lang$dom$Dom_LowLevel$onDocument = _elm_lang$dom$Native_Dom.onDocument;

var _elm_lang$core$Process$kill = _elm_lang$core$Native_Scheduler.kill;
var _elm_lang$core$Process$sleep = _elm_lang$core$Native_Scheduler.sleep;
var _elm_lang$core$Process$spawn = _elm_lang$core$Native_Scheduler.spawn;

var _elm_lang$mouse$Mouse_ops = _elm_lang$mouse$Mouse_ops || {};
_elm_lang$mouse$Mouse_ops['&>'] = F2(
	function (t1, t2) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (_p0) {
				return t2;
			},
			t1);
	});
var _elm_lang$mouse$Mouse$onSelfMsg = F3(
	function (router, _p1, state) {
		var _p2 = _p1;
		var _p3 = A2(_elm_lang$core$Dict$get, _p2.category, state);
		if (_p3.ctor === 'Nothing') {
			return _elm_lang$core$Task$succeed(state);
		} else {
			var send = function (tagger) {
				return A2(
					_elm_lang$core$Platform$sendToApp,
					router,
					tagger(_p2.position));
			};
			return A2(
				_elm_lang$mouse$Mouse_ops['&>'],
				_elm_lang$core$Task$sequence(
					A2(_elm_lang$core$List$map, send, _p3._0.taggers)),
				_elm_lang$core$Task$succeed(state));
		}
	});
var _elm_lang$mouse$Mouse$init = _elm_lang$core$Task$succeed(_elm_lang$core$Dict$empty);
var _elm_lang$mouse$Mouse$categorizeHelpHelp = F2(
	function (value, maybeValues) {
		var _p4 = maybeValues;
		if (_p4.ctor === 'Nothing') {
			return _elm_lang$core$Maybe$Just(
				{
					ctor: '::',
					_0: value,
					_1: {ctor: '[]'}
				});
		} else {
			return _elm_lang$core$Maybe$Just(
				{ctor: '::', _0: value, _1: _p4._0});
		}
	});
var _elm_lang$mouse$Mouse$categorizeHelp = F2(
	function (subs, subDict) {
		categorizeHelp:
		while (true) {
			var _p5 = subs;
			if (_p5.ctor === '[]') {
				return subDict;
			} else {
				var _v4 = _p5._1,
					_v5 = A3(
					_elm_lang$core$Dict$update,
					_p5._0._0,
					_elm_lang$mouse$Mouse$categorizeHelpHelp(_p5._0._1),
					subDict);
				subs = _v4;
				subDict = _v5;
				continue categorizeHelp;
			}
		}
	});
var _elm_lang$mouse$Mouse$categorize = function (subs) {
	return A2(_elm_lang$mouse$Mouse$categorizeHelp, subs, _elm_lang$core$Dict$empty);
};
var _elm_lang$mouse$Mouse$subscription = _elm_lang$core$Native_Platform.leaf('Mouse');
var _elm_lang$mouse$Mouse$Position = F2(
	function (a, b) {
		return {x: a, y: b};
	});
var _elm_lang$mouse$Mouse$position = A3(
	_elm_lang$core$Json_Decode$map2,
	_elm_lang$mouse$Mouse$Position,
	A2(_elm_lang$core$Json_Decode$field, 'pageX', _elm_lang$core$Json_Decode$int),
	A2(_elm_lang$core$Json_Decode$field, 'pageY', _elm_lang$core$Json_Decode$int));
var _elm_lang$mouse$Mouse$Watcher = F2(
	function (a, b) {
		return {taggers: a, pid: b};
	});
var _elm_lang$mouse$Mouse$Msg = F2(
	function (a, b) {
		return {category: a, position: b};
	});
var _elm_lang$mouse$Mouse$onEffects = F3(
	function (router, newSubs, oldState) {
		var rightStep = F3(
			function (category, taggers, task) {
				var tracker = A3(
					_elm_lang$dom$Dom_LowLevel$onDocument,
					category,
					_elm_lang$mouse$Mouse$position,
					function (_p6) {
						return A2(
							_elm_lang$core$Platform$sendToSelf,
							router,
							A2(_elm_lang$mouse$Mouse$Msg, category, _p6));
					});
				return A2(
					_elm_lang$core$Task$andThen,
					function (state) {
						return A2(
							_elm_lang$core$Task$andThen,
							function (pid) {
								return _elm_lang$core$Task$succeed(
									A3(
										_elm_lang$core$Dict$insert,
										category,
										A2(_elm_lang$mouse$Mouse$Watcher, taggers, pid),
										state));
							},
							_elm_lang$core$Process$spawn(tracker));
					},
					task);
			});
		var bothStep = F4(
			function (category, _p7, taggers, task) {
				var _p8 = _p7;
				return A2(
					_elm_lang$core$Task$andThen,
					function (state) {
						return _elm_lang$core$Task$succeed(
							A3(
								_elm_lang$core$Dict$insert,
								category,
								A2(_elm_lang$mouse$Mouse$Watcher, taggers, _p8.pid),
								state));
					},
					task);
			});
		var leftStep = F3(
			function (category, _p9, task) {
				var _p10 = _p9;
				return A2(
					_elm_lang$mouse$Mouse_ops['&>'],
					_elm_lang$core$Process$kill(_p10.pid),
					task);
			});
		return A6(
			_elm_lang$core$Dict$merge,
			leftStep,
			bothStep,
			rightStep,
			oldState,
			_elm_lang$mouse$Mouse$categorize(newSubs),
			_elm_lang$core$Task$succeed(_elm_lang$core$Dict$empty));
	});
var _elm_lang$mouse$Mouse$MySub = F2(
	function (a, b) {
		return {ctor: 'MySub', _0: a, _1: b};
	});
var _elm_lang$mouse$Mouse$clicks = function (tagger) {
	return _elm_lang$mouse$Mouse$subscription(
		A2(_elm_lang$mouse$Mouse$MySub, 'click', tagger));
};
var _elm_lang$mouse$Mouse$moves = function (tagger) {
	return _elm_lang$mouse$Mouse$subscription(
		A2(_elm_lang$mouse$Mouse$MySub, 'mousemove', tagger));
};
var _elm_lang$mouse$Mouse$downs = function (tagger) {
	return _elm_lang$mouse$Mouse$subscription(
		A2(_elm_lang$mouse$Mouse$MySub, 'mousedown', tagger));
};
var _elm_lang$mouse$Mouse$ups = function (tagger) {
	return _elm_lang$mouse$Mouse$subscription(
		A2(_elm_lang$mouse$Mouse$MySub, 'mouseup', tagger));
};
var _elm_lang$mouse$Mouse$subMap = F2(
	function (func, _p11) {
		var _p12 = _p11;
		return A2(
			_elm_lang$mouse$Mouse$MySub,
			_p12._0,
			function (_p13) {
				return func(
					_p12._1(_p13));
			});
	});
_elm_lang$core$Native_Platform.effectManagers['Mouse'] = {pkg: 'elm-lang/mouse', init: _elm_lang$mouse$Mouse$init, onEffects: _elm_lang$mouse$Mouse$onEffects, onSelfMsg: _elm_lang$mouse$Mouse$onSelfMsg, tag: 'sub', subMap: _elm_lang$mouse$Mouse$subMap};

var _debois$elm_mdl$Material_Helpers$noAttr = A2(_elm_lang$html$Html_Attributes$attribute, 'data-elm-mdl-noop', '');
var _debois$elm_mdl$Material_Helpers$aria = F2(
	function (name, value) {
		return value ? A2(
			_elm_lang$html$Html_Attributes$attribute,
			A2(_elm_lang$core$Basics_ops['++'], 'aria-', name),
			'true') : _debois$elm_mdl$Material_Helpers$noAttr;
	});
var _debois$elm_mdl$Material_Helpers$delay = F2(
	function (t, x) {
		return A2(
			_elm_lang$core$Task$perform,
			_elm_lang$core$Basics$always(x),
			_elm_lang$core$Process$sleep(t));
	});
var _debois$elm_mdl$Material_Helpers$cssTransitionStep = function (x) {
	return A2(_debois$elm_mdl$Material_Helpers$delay, 50, x);
};
var _debois$elm_mdl$Material_Helpers$cmd = function (msg) {
	return A2(
		_elm_lang$core$Task$perform,
		_elm_lang$core$Basics$always(msg),
		_elm_lang$core$Task$succeed(msg));
};
var _debois$elm_mdl$Material_Helpers$lift = F6(
	function (get, set, fwd, update, action, model) {
		var _p0 = A2(
			update,
			action,
			get(model));
		var submodel_ = _p0._0;
		var e = _p0._1;
		return {
			ctor: '_Tuple2',
			_0: A2(set, model, submodel_),
			_1: A2(_elm_lang$core$Platform_Cmd$map, fwd, e)
		};
	});
var _debois$elm_mdl$Material_Helpers$lift_ = F5(
	function (get, set, update, action, model) {
		return {
			ctor: '_Tuple2',
			_0: A2(
				set,
				model,
				A2(
					update,
					action,
					get(model))),
			_1: _elm_lang$core$Platform_Cmd$none
		};
	});
var _debois$elm_mdl$Material_Helpers$map2nd = F2(
	function (f, _p1) {
		var _p2 = _p1;
		return {
			ctor: '_Tuple2',
			_0: _p2._0,
			_1: f(_p2._1)
		};
	});
var _debois$elm_mdl$Material_Helpers$map1st = F2(
	function (f, _p3) {
		var _p4 = _p3;
		return {
			ctor: '_Tuple2',
			_0: f(_p4._0),
			_1: _p4._1
		};
	});
var _debois$elm_mdl$Material_Helpers$blurOn = function (evt) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		A2(_elm_lang$core$Basics_ops['++'], 'on', evt),
		'this.blur()');
};
var _debois$elm_mdl$Material_Helpers$effect = F2(
	function (e, x) {
		return {ctor: '_Tuple2', _0: x, _1: e};
	});
var _debois$elm_mdl$Material_Helpers$pure = _debois$elm_mdl$Material_Helpers$effect(_elm_lang$core$Platform_Cmd$none);
var _debois$elm_mdl$Material_Helpers$filter = F3(
	function (elem, attr, html) {
		return A2(
			elem,
			attr,
			A2(
				_elm_lang$core$List$filterMap,
				function (x) {
					return x;
				},
				html));
	});

var _debois$elm_mdl$Material_Component$subs = F5(
	function (ctor, get, subscriptions, lift, model) {
		return _elm_lang$core$Platform_Sub$batch(
			A3(
				_elm_lang$core$Dict$foldl,
				F3(
					function (idx, model, ss) {
						return {
							ctor: '::',
							_0: A2(
								_elm_lang$core$Platform_Sub$map,
								function (_p0) {
									return lift(
										A2(ctor, idx, _p0));
								},
								subscriptions(model)),
							_1: ss
						};
					}),
				{ctor: '[]'},
				get(model)));
	});
var _debois$elm_mdl$Material_Component$generalise = F4(
	function (update, lift, msg, model) {
		return A2(
			_debois$elm_mdl$Material_Helpers$map2nd,
			_elm_lang$core$Platform_Cmd$map(lift),
			A2(
				_debois$elm_mdl$Material_Helpers$map1st,
				_elm_lang$core$Maybe$Just,
				A2(update, msg, model)));
	});
var _debois$elm_mdl$Material_Component$react = F8(
	function (get, set, ctor, update, lift, msg, idx, store) {
		return A2(
			_debois$elm_mdl$Material_Helpers$map1st,
			_elm_lang$core$Maybe$map(
				A2(set, idx, store)),
			A3(
				update,
				function (_p1) {
					return lift(
						A2(ctor, idx, _p1));
				},
				msg,
				A2(get, idx, store)));
	});
var _debois$elm_mdl$Material_Component$react1 = F7(
	function (get, set, ctor, update, lift, msg, store) {
		return A2(
			_debois$elm_mdl$Material_Helpers$map1st,
			_elm_lang$core$Maybe$map(
				set(store)),
			A3(
				update,
				function (_p2) {
					return lift(
						ctor(_p2));
				},
				msg,
				get(store)));
	});
var _debois$elm_mdl$Material_Component$render = F6(
	function (get_model, view, ctor, lift, idx, store) {
		return A2(
			view,
			function (_p3) {
				return lift(
					A2(ctor, idx, _p3));
			},
			A2(get_model, idx, store));
	});
var _debois$elm_mdl$Material_Component$render1 = F5(
	function (get_model, view, ctor, lift, store) {
		return A2(
			view,
			function (_p4) {
				return lift(
					ctor(_p4));
			},
			get_model(store));
	});
var _debois$elm_mdl$Material_Component$indexed = F3(
	function (get_model, set_model, model0) {
		var set_ = F3(
			function (idx, store, model) {
				return A2(
					set_model,
					A3(
						_elm_lang$core$Dict$insert,
						idx,
						model,
						get_model(store)),
					store);
			});
		var get_ = F2(
			function (idx, store) {
				return A2(
					_elm_lang$core$Maybe$withDefault,
					model0,
					A2(
						_elm_lang$core$Dict$get,
						idx,
						get_model(store)));
			});
		return {ctor: '_Tuple2', _0: get_, _1: set_};
	});
var _debois$elm_mdl$Material_Component$Dispatch = function (a) {
	return {ctor: 'Dispatch', _0: a};
};
var _debois$elm_mdl$Material_Component$TabsMsg = F2(
	function (a, b) {
		return {ctor: 'TabsMsg', _0: a, _1: b};
	});
var _debois$elm_mdl$Material_Component$TooltipMsg = F2(
	function (a, b) {
		return {ctor: 'TooltipMsg', _0: a, _1: b};
	});
var _debois$elm_mdl$Material_Component$TogglesMsg = F2(
	function (a, b) {
		return {ctor: 'TogglesMsg', _0: a, _1: b};
	});
var _debois$elm_mdl$Material_Component$LayoutMsg = function (a) {
	return {ctor: 'LayoutMsg', _0: a};
};
var _debois$elm_mdl$Material_Component$MenuMsg = F2(
	function (a, b) {
		return {ctor: 'MenuMsg', _0: a, _1: b};
	});
var _debois$elm_mdl$Material_Component$TextfieldMsg = F2(
	function (a, b) {
		return {ctor: 'TextfieldMsg', _0: a, _1: b};
	});
var _debois$elm_mdl$Material_Component$ButtonMsg = F2(
	function (a, b) {
		return {ctor: 'ButtonMsg', _0: a, _1: b};
	});

var _debois$elm_mdl$Material_Dispatch$split = F4(
	function (k0, same, differ, xs) {
		split:
		while (true) {
			var _p0 = xs;
			if (_p0.ctor === '[]') {
				return {ctor: '_Tuple2', _0: same, _1: differ};
			} else {
				var _p1 = _p0._1;
				if (_elm_lang$core$Native_Utils.eq(_p0._0._0, k0)) {
					var _v1 = k0,
						_v2 = {ctor: '::', _0: _p0._0._1, _1: same},
						_v3 = differ,
						_v4 = _p1;
					k0 = _v1;
					same = _v2;
					differ = _v3;
					xs = _v4;
					continue split;
				} else {
					var _v5 = k0,
						_v6 = same,
						_v7 = {ctor: '::', _0: _p0._0, _1: differ},
						_v8 = _p1;
					k0 = _v5;
					same = _v6;
					differ = _v7;
					xs = _v8;
					continue split;
				}
			}
		}
	});
var _debois$elm_mdl$Material_Dispatch$group_ = F2(
	function (acc, items) {
		group_:
		while (true) {
			var _p2 = items;
			if (_p2.ctor === '[]') {
				return acc;
			} else {
				if (_p2._1.ctor === '[]') {
					return {
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: _p2._0._0,
							_1: {
								ctor: '::',
								_0: _p2._0._1,
								_1: {ctor: '[]'}
							}
						},
						_1: acc
					};
				} else {
					if ((_p2._1._0.ctor === '_Tuple2') && (_p2._1._1.ctor === '[]')) {
						var _p6 = _p2._1._0._1;
						var _p5 = _p2._0._1;
						var _p4 = _p2._1._0._0;
						var _p3 = _p2._0._0;
						return _elm_lang$core$Native_Utils.eq(_p3, _p4) ? {
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: _p3,
								_1: {
									ctor: '::',
									_0: _p6,
									_1: {
										ctor: '::',
										_0: _p5,
										_1: {ctor: '[]'}
									}
								}
							},
							_1: acc
						} : {
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: _p4,
								_1: {
									ctor: '::',
									_0: _p6,
									_1: {ctor: '[]'}
								}
							},
							_1: {
								ctor: '::',
								_0: {
									ctor: '_Tuple2',
									_0: _p3,
									_1: {
										ctor: '::',
										_0: _p5,
										_1: {ctor: '[]'}
									}
								},
								_1: acc
							}
						};
					} else {
						var _p8 = _p2._0._0;
						var _p7 = A4(
							_debois$elm_mdl$Material_Dispatch$split,
							_p8,
							{
								ctor: '::',
								_0: _p2._0._1,
								_1: {ctor: '[]'}
							},
							{ctor: '[]'},
							_p2._1);
						var same = _p7._0;
						var different = _p7._1;
						var _v10 = {
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: _p8, _1: same},
							_1: acc
						},
							_v11 = different;
						acc = _v10;
						items = _v11;
						continue group_;
					}
				}
			}
		}
	});
var _debois$elm_mdl$Material_Dispatch$group = _debois$elm_mdl$Material_Dispatch$group_(
	{ctor: '[]'});
var _debois$elm_mdl$Material_Dispatch$onSingle = function (_p9) {
	var _p10 = _p9;
	return A3(
		_elm_lang$html$Html_Events$onWithOptions,
		_p10._0,
		A2(_elm_lang$core$Maybe$withDefault, _elm_lang$html$Html_Events$defaultOptions, _p10._1._1),
		_p10._1._0);
};
var _debois$elm_mdl$Material_Dispatch$pickOptions = function (decoders) {
	pickOptions:
	while (true) {
		var _p11 = decoders;
		if (_p11.ctor === '::') {
			if ((_p11._0.ctor === '_Tuple2') && (_p11._0._1.ctor === 'Just')) {
				return _p11._0._1._0;
			} else {
				var _v14 = _p11._1;
				decoders = _v14;
				continue pickOptions;
			}
		} else {
			return _elm_lang$html$Html_Events$defaultOptions;
		}
	}
};
var _debois$elm_mdl$Material_Dispatch$flatten = function (decoders) {
	return A2(
		_elm_lang$core$Json_Decode$map,
		function (value) {
			return A2(
				_elm_lang$core$List$filterMap,
				function (decoder) {
					return _elm_lang$core$Result$toMaybe(
						A2(_elm_lang$core$Json_Decode$decodeValue, decoder, value));
				},
				decoders);
		},
		_elm_lang$core$Json_Decode$value);
};
var _debois$elm_mdl$Material_Dispatch$onWithOptions = F4(
	function (event, lift, options, decoders) {
		return A3(
			_elm_lang$html$Html_Events$onWithOptions,
			event,
			options,
			A2(
				_elm_lang$core$Json_Decode$map,
				lift,
				_debois$elm_mdl$Material_Dispatch$flatten(decoders)));
	});
var _debois$elm_mdl$Material_Dispatch$on = F2(
	function (event, lift) {
		return A3(_debois$elm_mdl$Material_Dispatch$onWithOptions, event, lift, _elm_lang$html$Html_Events$defaultOptions);
	});
var _debois$elm_mdl$Material_Dispatch$onMany = F2(
	function (lift, decoders) {
		var _p12 = decoders;
		if ((_p12._1.ctor === '::') && (_p12._1._1.ctor === '[]')) {
			return _debois$elm_mdl$Material_Dispatch$onSingle(
				{ctor: '_Tuple2', _0: _p12._0, _1: _p12._1._0});
		} else {
			var _p13 = _p12._1;
			return A3(
				_elm_lang$html$Html_Events$onWithOptions,
				_p12._0,
				_debois$elm_mdl$Material_Dispatch$pickOptions(_p13),
				lift(
					_debois$elm_mdl$Material_Dispatch$flatten(
						A2(_elm_lang$core$List$map, _elm_lang$core$Tuple$first, _p13))));
		}
	});
var _debois$elm_mdl$Material_Dispatch$map2nd = F2(
	function (f, _p14) {
		var _p15 = _p14;
		return {
			ctor: '_Tuple2',
			_0: _p15._0,
			_1: f(_p15._1)
		};
	});
var _debois$elm_mdl$Material_Dispatch$update1 = F3(
	function (update, cmd, _p16) {
		var _p17 = _p16;
		return A2(
			_debois$elm_mdl$Material_Dispatch$map2nd,
			A2(
				_elm_lang$core$Basics$flip,
				F2(
					function (x, y) {
						return {ctor: '::', _0: x, _1: y};
					}),
				_p17._1),
			A2(update, cmd, _p17._0));
	});
var _debois$elm_mdl$Material_Dispatch$update = F3(
	function (update, msg, model) {
		return A2(
			_debois$elm_mdl$Material_Dispatch$map2nd,
			_elm_lang$core$Platform_Cmd$batch,
			A3(
				_elm_lang$core$List$foldl,
				_debois$elm_mdl$Material_Dispatch$update1(update),
				{
					ctor: '_Tuple2',
					_0: model,
					_1: {ctor: '[]'}
				},
				msg));
	});
var _debois$elm_mdl$Material_Dispatch$cmd = function (msg) {
	return A2(
		_elm_lang$core$Task$perform,
		_elm_lang$core$Basics$always(msg),
		_elm_lang$core$Task$succeed(msg));
};
var _debois$elm_mdl$Material_Dispatch$forward = function (messages) {
	return _elm_lang$core$Platform_Cmd$batch(
		A2(_elm_lang$core$List$map, _debois$elm_mdl$Material_Dispatch$cmd, messages));
};
var _debois$elm_mdl$Material_Dispatch$toAttributes = function (_p18) {
	var _p19 = _p18;
	var _p21 = _p19._0;
	var _p20 = _p21.lift;
	if (_p20.ctor === 'Just') {
		return A2(
			_elm_lang$core$List$map,
			_debois$elm_mdl$Material_Dispatch$onMany(_p20._0),
			_debois$elm_mdl$Material_Dispatch$group(_p21.decoders));
	} else {
		return A2(_elm_lang$core$List$map, _debois$elm_mdl$Material_Dispatch$onSingle, _p21.decoders);
	}
};
var _debois$elm_mdl$Material_Dispatch$getDecoder = function (_p22) {
	var _p23 = _p22;
	return _p23._0.lift;
};
var _debois$elm_mdl$Material_Dispatch$Config = function (a) {
	return {ctor: 'Config', _0: a};
};
var _debois$elm_mdl$Material_Dispatch$defaultConfig = _debois$elm_mdl$Material_Dispatch$Config(
	{
		decoders: {ctor: '[]'},
		lift: _elm_lang$core$Maybe$Nothing
	});
var _debois$elm_mdl$Material_Dispatch$setDecoder = F2(
	function (f, _p24) {
		var _p25 = _p24;
		return _debois$elm_mdl$Material_Dispatch$Config(
			_elm_lang$core$Native_Utils.update(
				_p25._0,
				{
					lift: _elm_lang$core$Maybe$Just(f)
				}));
	});
var _debois$elm_mdl$Material_Dispatch$setMsg = function (_p26) {
	return _debois$elm_mdl$Material_Dispatch$setDecoder(
		_elm_lang$core$Json_Decode$map(_p26));
};
var _debois$elm_mdl$Material_Dispatch$add = F4(
	function (event, options, decoder, _p27) {
		var _p28 = _p27;
		var _p29 = _p28._0;
		return _debois$elm_mdl$Material_Dispatch$Config(
			_elm_lang$core$Native_Utils.update(
				_p29,
				{
					decoders: {
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: event,
							_1: {ctor: '_Tuple2', _0: decoder, _1: options}
						},
						_1: _p29.decoders
					}
				}));
	});
var _debois$elm_mdl$Material_Dispatch$clear = function (_p30) {
	var _p31 = _p30;
	return _debois$elm_mdl$Material_Dispatch$Config(
		_elm_lang$core$Native_Utils.update(
			_p31._0,
			{
				decoders: {ctor: '[]'}
			}));
};

var _debois$elm_mdl$Material_Options_Internal$addAttributes = F2(
	function (summary, attrs) {
		return A2(
			_elm_lang$core$Basics_ops['++'],
			summary.attrs,
			A2(
				_elm_lang$core$Basics_ops['++'],
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$style(summary.css),
					_1: {
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$class(
							A2(_elm_lang$core$String$join, ' ', summary.classes)),
						_1: {ctor: '[]'}
					}
				},
				A2(
					_elm_lang$core$Basics_ops['++'],
					attrs,
					A2(
						_elm_lang$core$Basics_ops['++'],
						summary.internal,
						_debois$elm_mdl$Material_Dispatch$toAttributes(summary.dispatch)))));
	});
var _debois$elm_mdl$Material_Options_Internal$collect1_ = F2(
	function (options, acc) {
		var _p0 = options;
		switch (_p0.ctor) {
			case 'Class':
				return _elm_lang$core$Native_Utils.update(
					acc,
					{
						classes: {ctor: '::', _0: _p0._0, _1: acc.classes}
					});
			case 'CSS':
				return _elm_lang$core$Native_Utils.update(
					acc,
					{
						css: {ctor: '::', _0: _p0._0, _1: acc.css}
					});
			case 'Attribute':
				return _elm_lang$core$Native_Utils.update(
					acc,
					{
						attrs: {ctor: '::', _0: _p0._0, _1: acc.attrs}
					});
			case 'Internal':
				return _elm_lang$core$Native_Utils.update(
					acc,
					{
						internal: {ctor: '::', _0: _p0._0, _1: acc.internal}
					});
			case 'Listener':
				return _elm_lang$core$Native_Utils.update(
					acc,
					{
						dispatch: A4(_debois$elm_mdl$Material_Dispatch$add, _p0._0, _p0._1, _p0._2, acc.dispatch)
					});
			case 'Many':
				return A3(_elm_lang$core$List$foldl, _debois$elm_mdl$Material_Options_Internal$collect1_, acc, _p0._0);
			case 'Lift':
				return _elm_lang$core$Native_Utils.update(
					acc,
					{
						dispatch: A2(_debois$elm_mdl$Material_Dispatch$setDecoder, _p0._0, acc.dispatch)
					});
			case 'Set':
				return acc;
			default:
				return acc;
		}
	});
var _debois$elm_mdl$Material_Options_Internal$collect1 = F2(
	function (option, acc) {
		var _p1 = option;
		switch (_p1.ctor) {
			case 'Class':
				return _elm_lang$core$Native_Utils.update(
					acc,
					{
						classes: {ctor: '::', _0: _p1._0, _1: acc.classes}
					});
			case 'CSS':
				return _elm_lang$core$Native_Utils.update(
					acc,
					{
						css: {ctor: '::', _0: _p1._0, _1: acc.css}
					});
			case 'Attribute':
				return _elm_lang$core$Native_Utils.update(
					acc,
					{
						attrs: {ctor: '::', _0: _p1._0, _1: acc.attrs}
					});
			case 'Internal':
				return _elm_lang$core$Native_Utils.update(
					acc,
					{
						internal: {ctor: '::', _0: _p1._0, _1: acc.internal}
					});
			case 'Many':
				return A3(_elm_lang$core$List$foldl, _debois$elm_mdl$Material_Options_Internal$collect1, acc, _p1._0);
			case 'Set':
				return _elm_lang$core$Native_Utils.update(
					acc,
					{
						config: _p1._0(acc.config)
					});
			case 'Listener':
				return _elm_lang$core$Native_Utils.update(
					acc,
					{
						dispatch: A4(_debois$elm_mdl$Material_Dispatch$add, _p1._0, _p1._1, _p1._2, acc.dispatch)
					});
			case 'Lift':
				return _elm_lang$core$Native_Utils.update(
					acc,
					{
						dispatch: A2(_debois$elm_mdl$Material_Dispatch$setDecoder, _p1._0, acc.dispatch)
					});
			default:
				return acc;
		}
	});
var _debois$elm_mdl$Material_Options_Internal$recollect = _elm_lang$core$List$foldl(_debois$elm_mdl$Material_Options_Internal$collect1);
var _debois$elm_mdl$Material_Options_Internal$apply = F4(
	function (summary, ctor, options, attrs) {
		return ctor(
			A2(
				_debois$elm_mdl$Material_Options_Internal$addAttributes,
				A2(_debois$elm_mdl$Material_Options_Internal$recollect, summary, options),
				attrs));
	});
var _debois$elm_mdl$Material_Options_Internal$Summary = F6(
	function (a, b, c, d, e, f) {
		return {classes: a, css: b, attrs: c, internal: d, dispatch: e, config: f};
	});
var _debois$elm_mdl$Material_Options_Internal$collect = function (_p2) {
	return _debois$elm_mdl$Material_Options_Internal$recollect(
		A6(
			_debois$elm_mdl$Material_Options_Internal$Summary,
			{ctor: '[]'},
			{ctor: '[]'},
			{ctor: '[]'},
			{ctor: '[]'},
			_debois$elm_mdl$Material_Dispatch$defaultConfig,
			_p2));
};
var _debois$elm_mdl$Material_Options_Internal$collect_ = A2(
	_elm_lang$core$List$foldl,
	_debois$elm_mdl$Material_Options_Internal$collect1_,
	A6(
		_debois$elm_mdl$Material_Options_Internal$Summary,
		{ctor: '[]'},
		{ctor: '[]'},
		{ctor: '[]'},
		{ctor: '[]'},
		_debois$elm_mdl$Material_Dispatch$defaultConfig,
		{ctor: '_Tuple0'}));
var _debois$elm_mdl$Material_Options_Internal$None = {ctor: 'None'};
var _debois$elm_mdl$Material_Options_Internal$Lift = function (a) {
	return {ctor: 'Lift', _0: a};
};
var _debois$elm_mdl$Material_Options_Internal$dispatch = function (lift) {
	return _debois$elm_mdl$Material_Options_Internal$Lift(
		function (_p3) {
			return A2(
				_elm_lang$core$Json_Decode$map,
				lift,
				A2(_elm_lang$core$Json_Decode$map, _debois$elm_mdl$Material_Component$Dispatch, _p3));
		});
};
var _debois$elm_mdl$Material_Options_Internal$inject = F5(
	function (view, lift, a, b, c) {
		return A3(
			view,
			a,
			b,
			{
				ctor: '::',
				_0: _debois$elm_mdl$Material_Options_Internal$dispatch(lift),
				_1: c
			});
	});
var _debois$elm_mdl$Material_Options_Internal$Listener = F3(
	function (a, b, c) {
		return {ctor: 'Listener', _0: a, _1: b, _2: c};
	});
var _debois$elm_mdl$Material_Options_Internal$on1 = F3(
	function (event, lift, m) {
		return A3(
			_debois$elm_mdl$Material_Options_Internal$Listener,
			event,
			_elm_lang$core$Maybe$Nothing,
			A2(
				_elm_lang$core$Json_Decode$map,
				lift,
				_elm_lang$core$Json_Decode$succeed(m)));
	});
var _debois$elm_mdl$Material_Options_Internal$Set = function (a) {
	return {ctor: 'Set', _0: a};
};
var _debois$elm_mdl$Material_Options_Internal$option = _debois$elm_mdl$Material_Options_Internal$Set;
var _debois$elm_mdl$Material_Options_Internal$Many = function (a) {
	return {ctor: 'Many', _0: a};
};
var _debois$elm_mdl$Material_Options_Internal$applyContainer = F3(
	function (summary, ctor, options) {
		return A4(
			_debois$elm_mdl$Material_Options_Internal$apply,
			_elm_lang$core$Native_Utils.update(
				summary,
				{
					dispatch: _debois$elm_mdl$Material_Dispatch$clear(summary.dispatch),
					attrs: {ctor: '[]'},
					internal: {ctor: '[]'},
					config: {ctor: '_Tuple0'}
				}),
			ctor,
			{
				ctor: '::',
				_0: _debois$elm_mdl$Material_Options_Internal$Many(summary.config.container),
				_1: options
			},
			{ctor: '[]'});
	});
var _debois$elm_mdl$Material_Options_Internal$applyInput = F3(
	function (summary, ctor, options) {
		return A4(
			_debois$elm_mdl$Material_Options_Internal$apply,
			_elm_lang$core$Native_Utils.update(
				summary,
				{
					classes: {ctor: '[]'},
					css: {ctor: '[]'},
					config: {ctor: '_Tuple0'}
				}),
			ctor,
			{
				ctor: '::',
				_0: _debois$elm_mdl$Material_Options_Internal$Many(summary.config.input),
				_1: options
			},
			{ctor: '[]'});
	});
var _debois$elm_mdl$Material_Options_Internal$input = function (_p4) {
	return _debois$elm_mdl$Material_Options_Internal$option(
		F2(
			function (style, config) {
				return _elm_lang$core$Native_Utils.update(
					config,
					{
						input: {
							ctor: '::',
							_0: _debois$elm_mdl$Material_Options_Internal$Many(style),
							_1: config.input
						}
					});
			})(_p4));
};
var _debois$elm_mdl$Material_Options_Internal$container = function (_p5) {
	return _debois$elm_mdl$Material_Options_Internal$option(
		F2(
			function (style, config) {
				return _elm_lang$core$Native_Utils.update(
					config,
					{
						container: {
							ctor: '::',
							_0: _debois$elm_mdl$Material_Options_Internal$Many(style),
							_1: config.container
						}
					});
			})(_p5));
};
var _debois$elm_mdl$Material_Options_Internal$Internal = function (a) {
	return {ctor: 'Internal', _0: a};
};
var _debois$elm_mdl$Material_Options_Internal$attribute = _debois$elm_mdl$Material_Options_Internal$Internal;
var _debois$elm_mdl$Material_Options_Internal$Attribute = function (a) {
	return {ctor: 'Attribute', _0: a};
};
var _debois$elm_mdl$Material_Options_Internal$CSS = function (a) {
	return {ctor: 'CSS', _0: a};
};
var _debois$elm_mdl$Material_Options_Internal$Class = function (a) {
	return {ctor: 'Class', _0: a};
};

var _debois$elm_mdl$Material_Options$dispatch = function (_p0) {
	return _debois$elm_mdl$Material_Options_Internal$Lift(
		_elm_lang$core$Json_Decode$map(_p0));
};
var _debois$elm_mdl$Material_Options$onWithOptions = F2(
	function (evt, options) {
		return A2(
			_debois$elm_mdl$Material_Options_Internal$Listener,
			evt,
			_elm_lang$core$Maybe$Just(options));
	});
var _debois$elm_mdl$Material_Options$on = function (event) {
	return A2(_debois$elm_mdl$Material_Options_Internal$Listener, event, _elm_lang$core$Maybe$Nothing);
};
var _debois$elm_mdl$Material_Options$on1 = F2(
	function (event, m) {
		return A2(
			_debois$elm_mdl$Material_Options$on,
			event,
			_elm_lang$core$Json_Decode$succeed(m));
	});
var _debois$elm_mdl$Material_Options$onToggle = _debois$elm_mdl$Material_Options$on1('change');
var _debois$elm_mdl$Material_Options$onClick = function (msg) {
	return A2(
		_debois$elm_mdl$Material_Options$on,
		'click',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _debois$elm_mdl$Material_Options$onDoubleClick = function (msg) {
	return A2(
		_debois$elm_mdl$Material_Options$on,
		'dblclick',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _debois$elm_mdl$Material_Options$onMouseDown = function (msg) {
	return A2(
		_debois$elm_mdl$Material_Options$on,
		'mousedown',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _debois$elm_mdl$Material_Options$onMouseUp = function (msg) {
	return A2(
		_debois$elm_mdl$Material_Options$on,
		'mouseup',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _debois$elm_mdl$Material_Options$onMouseEnter = function (msg) {
	return A2(
		_debois$elm_mdl$Material_Options$on,
		'mouseenter',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _debois$elm_mdl$Material_Options$onMouseLeave = function (msg) {
	return A2(
		_debois$elm_mdl$Material_Options$on,
		'mouseleave',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _debois$elm_mdl$Material_Options$onMouseOver = function (msg) {
	return A2(
		_debois$elm_mdl$Material_Options$on,
		'mouseover',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _debois$elm_mdl$Material_Options$onMouseOut = function (msg) {
	return A2(
		_debois$elm_mdl$Material_Options$on,
		'mouseout',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _debois$elm_mdl$Material_Options$onCheck = function (_p1) {
	return A2(
		_debois$elm_mdl$Material_Options$on,
		'change',
		A3(_elm_lang$core$Basics$flip, _elm_lang$core$Json_Decode$map, _elm_lang$html$Html_Events$targetChecked, _p1));
};
var _debois$elm_mdl$Material_Options$onBlur = function (msg) {
	return A2(
		_debois$elm_mdl$Material_Options$on,
		'blur',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _debois$elm_mdl$Material_Options$onFocus = function (msg) {
	return A2(
		_debois$elm_mdl$Material_Options$on,
		'focus',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _debois$elm_mdl$Material_Options$onInput = function (f) {
	return A2(
		_debois$elm_mdl$Material_Options$on,
		'input',
		A2(_elm_lang$core$Json_Decode$map, f, _elm_lang$html$Html_Events$targetValue));
};
var _debois$elm_mdl$Material_Options$container = _debois$elm_mdl$Material_Options_Internal$container;
var _debois$elm_mdl$Material_Options$input = _debois$elm_mdl$Material_Options_Internal$input;
var _debois$elm_mdl$Material_Options$id = function (_p2) {
	return _debois$elm_mdl$Material_Options_Internal$Attribute(
		_elm_lang$html$Html_Attributes$id(_p2));
};
var _debois$elm_mdl$Material_Options$attr = _debois$elm_mdl$Material_Options_Internal$Attribute;
var _debois$elm_mdl$Material_Options$attribute = _debois$elm_mdl$Material_Options_Internal$Attribute;
var _debois$elm_mdl$Material_Options$stylesheet = function (css) {
	return A3(
		_elm_lang$html$Html$node,
		'style',
		{ctor: '[]'},
		{
			ctor: '::',
			_0: _elm_lang$html$Html$text(css),
			_1: {ctor: '[]'}
		});
};
var _debois$elm_mdl$Material_Options$data = F2(
	function (key, val) {
		return _debois$elm_mdl$Material_Options_Internal$Attribute(
			A2(
				_elm_lang$html$Html_Attributes$attribute,
				A2(_elm_lang$core$Basics_ops['++'], 'data-', key),
				val));
	});
var _debois$elm_mdl$Material_Options$nop = _debois$elm_mdl$Material_Options_Internal$None;
var _debois$elm_mdl$Material_Options$when = F2(
	function (guard, prop) {
		return guard ? prop : _debois$elm_mdl$Material_Options$nop;
	});
var _debois$elm_mdl$Material_Options$maybe = function (prop) {
	return A2(_elm_lang$core$Maybe$withDefault, _debois$elm_mdl$Material_Options$nop, prop);
};
var _debois$elm_mdl$Material_Options$many = _debois$elm_mdl$Material_Options_Internal$Many;
var _debois$elm_mdl$Material_Options$css = F2(
	function (key, value) {
		return _debois$elm_mdl$Material_Options_Internal$CSS(
			{ctor: '_Tuple2', _0: key, _1: value});
	});
var _debois$elm_mdl$Material_Options$center = _debois$elm_mdl$Material_Options$many(
	{
		ctor: '::',
		_0: A2(_debois$elm_mdl$Material_Options$css, 'display', 'flex'),
		_1: {
			ctor: '::',
			_0: A2(_debois$elm_mdl$Material_Options$css, 'align-items', 'center'),
			_1: {
				ctor: '::',
				_0: A2(_debois$elm_mdl$Material_Options$css, 'justify-content', 'center'),
				_1: {ctor: '[]'}
			}
		}
	});
var _debois$elm_mdl$Material_Options$scrim = function (opacity) {
	return A2(
		_debois$elm_mdl$Material_Options$css,
		'background',
		A2(
			_elm_lang$core$Basics_ops['++'],
			'linear-gradient(rgba(0, 0, 0, 0), rgba(0, 0, 0, ',
			A2(
				_elm_lang$core$Basics_ops['++'],
				_elm_lang$core$Basics$toString(opacity),
				'))')));
};
var _debois$elm_mdl$Material_Options$cs = function (c) {
	return _debois$elm_mdl$Material_Options_Internal$Class(c);
};
var _debois$elm_mdl$Material_Options$disabled = function (v) {
	return _debois$elm_mdl$Material_Options_Internal$Attribute(
		_elm_lang$html$Html_Attributes$disabled(v));
};
var _debois$elm_mdl$Material_Options$styled_ = F3(
	function (ctor, props, attrs) {
		return ctor(
			A2(
				_debois$elm_mdl$Material_Options_Internal$addAttributes,
				_debois$elm_mdl$Material_Options_Internal$collect_(props),
				attrs));
	});
var _debois$elm_mdl$Material_Options$img = F2(
	function (options, attrs) {
		return A4(
			_debois$elm_mdl$Material_Options$styled_,
			_elm_lang$html$Html$img,
			options,
			attrs,
			{ctor: '[]'});
	});
var _debois$elm_mdl$Material_Options$styled = F2(
	function (ctor, props) {
		return ctor(
			A2(
				_debois$elm_mdl$Material_Options_Internal$addAttributes,
				_debois$elm_mdl$Material_Options_Internal$collect_(props),
				{ctor: '[]'}));
	});
var _debois$elm_mdl$Material_Options$div = _debois$elm_mdl$Material_Options$styled(_elm_lang$html$Html$div);
var _debois$elm_mdl$Material_Options$span = _debois$elm_mdl$Material_Options$styled(_elm_lang$html$Html$span);

var _debois$elm_mdl$Material_Ripple$styles = F2(
	function (m, frame) {
		var r = m.rect;
		var toPx = function (k) {
			return A2(
				_elm_lang$core$Basics_ops['++'],
				_elm_lang$core$Basics$toString(
					_elm_lang$core$Basics$round(k)),
				'px');
		};
		var offset = A2(
			_elm_lang$core$Basics_ops['++'],
			'translate(',
			A2(
				_elm_lang$core$Basics_ops['++'],
				toPx(m.x),
				A2(
					_elm_lang$core$Basics_ops['++'],
					', ',
					A2(
						_elm_lang$core$Basics_ops['++'],
						toPx(m.y),
						')'))));
		var rippleSize = toPx(
			(_elm_lang$core$Basics$sqrt((r.width * r.width) + (r.height * r.height)) * 2.0) + 2.0);
		var scale = _elm_lang$core$Native_Utils.eq(frame, 0) ? 'scale(0.0001, 0.0001)' : '';
		var transformString = A2(
			_elm_lang$core$Basics_ops['++'],
			'translate(-50%, -50%) ',
			A2(_elm_lang$core$Basics_ops['++'], offset, scale));
		return {
			ctor: '::',
			_0: {ctor: '_Tuple2', _0: 'width', _1: rippleSize},
			_1: {
				ctor: '::',
				_0: {ctor: '_Tuple2', _0: 'height', _1: rippleSize},
				_1: {
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: '-webkit-transform', _1: transformString},
					_1: {
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: '-ms-transform', _1: transformString},
						_1: {
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'transform', _1: transformString},
							_1: {ctor: '[]'}
						}
					}
				}
			}
		};
	});
var _debois$elm_mdl$Material_Ripple$Metrics = F3(
	function (a, b, c) {
		return {rect: a, x: b, y: c};
	});
var _debois$elm_mdl$Material_Ripple$computeMetrics = function (g) {
	var rect = g.rect;
	var set = F2(
		function (x, y) {
			return _elm_lang$core$Maybe$Just(
				{ctor: '_Tuple2', _0: x - rect.left, _1: y - rect.top});
		});
	return A2(
		_elm_lang$core$Maybe$map,
		function (_p0) {
			var _p1 = _p0;
			return A3(_debois$elm_mdl$Material_Ripple$Metrics, rect, _p1._0, _p1._1);
		},
		function () {
			var _p2 = {ctor: '_Tuple4', _0: g.clientX, _1: g.clientY, _2: g.touchX, _3: g.touchY};
			_v1_3:
			do {
				if (_p2.ctor === '_Tuple4') {
					if ((_p2._0.ctor === 'Just') && (_p2._1.ctor === 'Just')) {
						if ((_p2._0._0 === 0.0) && (_p2._1._0 === 0.0)) {
							return _elm_lang$core$Maybe$Just(
								{ctor: '_Tuple2', _0: rect.width / 2.0, _1: rect.height / 2.0});
						} else {
							return A2(set, _p2._0._0, _p2._1._0);
						}
					} else {
						if ((_p2._2.ctor === 'Just') && (_p2._3.ctor === 'Just')) {
							return A2(set, _p2._2._0, _p2._3._0);
						} else {
							break _v1_3;
						}
					}
				} else {
					break _v1_3;
				}
			} while(false);
			return _elm_lang$core$Maybe$Nothing;
		}());
};
var _debois$elm_mdl$Material_Ripple$Model = F3(
	function (a, b, c) {
		return {animation: a, metrics: b, ignoringMouseDown: c};
	});
var _debois$elm_mdl$Material_Ripple$DOMState = F6(
	function (a, b, c, d, e, f) {
		return {rect: a, clientX: b, clientY: c, touchX: d, touchY: e, type_: f};
	});
var _debois$elm_mdl$Material_Ripple$geometryDecoder = A7(
	_elm_lang$core$Json_Decode$map6,
	_debois$elm_mdl$Material_Ripple$DOMState,
	A2(_elm_lang$core$Json_Decode$field, 'currentTarget', _debois$elm_dom$DOM$boundingClientRect),
	_elm_lang$core$Json_Decode$maybe(
		A2(_elm_lang$core$Json_Decode$field, 'clientX', _elm_lang$core$Json_Decode$float)),
	_elm_lang$core$Json_Decode$maybe(
		A2(_elm_lang$core$Json_Decode$field, 'clientY', _elm_lang$core$Json_Decode$float)),
	_elm_lang$core$Json_Decode$maybe(
		A2(
			_elm_lang$core$Json_Decode$at,
			{
				ctor: '::',
				_0: 'touches',
				_1: {
					ctor: '::',
					_0: '0',
					_1: {
						ctor: '::',
						_0: 'clientX',
						_1: {ctor: '[]'}
					}
				}
			},
			_elm_lang$core$Json_Decode$float)),
	_elm_lang$core$Json_Decode$maybe(
		A2(
			_elm_lang$core$Json_Decode$at,
			{
				ctor: '::',
				_0: 'touches',
				_1: {
					ctor: '::',
					_0: '0',
					_1: {
						ctor: '::',
						_0: 'clientY',
						_1: {ctor: '[]'}
					}
				}
			},
			_elm_lang$core$Json_Decode$float)),
	A2(_elm_lang$core$Json_Decode$field, 'type', _elm_lang$core$Json_Decode$string));
var _debois$elm_mdl$Material_Ripple$Inert = {ctor: 'Inert'};
var _debois$elm_mdl$Material_Ripple$model = {animation: _debois$elm_mdl$Material_Ripple$Inert, metrics: _elm_lang$core$Maybe$Nothing, ignoringMouseDown: false};
var _debois$elm_mdl$Material_Ripple$Frame = function (a) {
	return {ctor: 'Frame', _0: a};
};
var _debois$elm_mdl$Material_Ripple$view_ = F2(
	function (attrs, model) {
		var styling = function () {
			var _p3 = {ctor: '_Tuple2', _0: model.metrics, _1: model.animation};
			if ((_p3.ctor === '_Tuple2') && (_p3._0.ctor === 'Just')) {
				if (_p3._1.ctor === 'Frame') {
					return A2(_debois$elm_mdl$Material_Ripple$styles, _p3._0._0, _p3._1._0);
				} else {
					return A2(_debois$elm_mdl$Material_Ripple$styles, _p3._0._0, 1);
				}
			} else {
				return {ctor: '[]'};
			}
		}();
		return A2(
			_elm_lang$html$Html$span,
			attrs,
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$span,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$classList(
							{
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: 'mdl-ripple', _1: true},
								_1: {
									ctor: '::',
									_0: {
										ctor: '_Tuple2',
										_0: 'is-animating',
										_1: !_elm_lang$core$Native_Utils.eq(
											model.animation,
											_debois$elm_mdl$Material_Ripple$Frame(0))
									},
									_1: {
										ctor: '::',
										_0: {
											ctor: '_Tuple2',
											_0: 'is-visible',
											_1: !_elm_lang$core$Native_Utils.eq(model.animation, _debois$elm_mdl$Material_Ripple$Inert)
										},
										_1: {ctor: '[]'}
									}
								}
							}),
						_1: {
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$style(styling),
							_1: {ctor: '[]'}
						}
					},
					{ctor: '[]'}),
				_1: {ctor: '[]'}
			});
	});
var _debois$elm_mdl$Material_Ripple$Tick = {ctor: 'Tick'};
var _debois$elm_mdl$Material_Ripple$update = F2(
	function (action, model) {
		var _p4 = action;
		switch (_p4.ctor) {
			case 'Down':
				var _p5 = _p4._0;
				return (_elm_lang$core$Native_Utils.eq(_p5.type_, 'mousedown') && model.ignoringMouseDown) ? _debois$elm_mdl$Material_Helpers$pure(
					_elm_lang$core$Native_Utils.update(
						model,
						{ignoringMouseDown: false})) : A2(
					_debois$elm_mdl$Material_Helpers$effect,
					_debois$elm_mdl$Material_Helpers$cssTransitionStep(_debois$elm_mdl$Material_Ripple$Tick),
					_elm_lang$core$Native_Utils.update(
						model,
						{
							animation: _debois$elm_mdl$Material_Ripple$Frame(0),
							metrics: _debois$elm_mdl$Material_Ripple$computeMetrics(_p5),
							ignoringMouseDown: _elm_lang$core$Native_Utils.eq(_p5.type_, 'touchstart') ? true : model.ignoringMouseDown
						}));
			case 'Up':
				return _debois$elm_mdl$Material_Helpers$pure(
					_elm_lang$core$Native_Utils.update(
						model,
						{animation: _debois$elm_mdl$Material_Ripple$Inert}));
			default:
				return _elm_lang$core$Native_Utils.eq(
					model.animation,
					_debois$elm_mdl$Material_Ripple$Frame(0)) ? _debois$elm_mdl$Material_Helpers$pure(
					_elm_lang$core$Native_Utils.update(
						model,
						{
							animation: _debois$elm_mdl$Material_Ripple$Frame(1)
						})) : _debois$elm_mdl$Material_Helpers$pure(model);
		}
	});
var _debois$elm_mdl$Material_Ripple$Up = {ctor: 'Up'};
var _debois$elm_mdl$Material_Ripple$up = F2(
	function (f, name) {
		return A2(
			_debois$elm_mdl$Material_Options$on,
			name,
			_elm_lang$core$Json_Decode$succeed(
				f(_debois$elm_mdl$Material_Ripple$Up)));
	});
var _debois$elm_mdl$Material_Ripple$upOn_ = F2(
	function (f, name) {
		return A2(
			_elm_lang$html$Html_Events$on,
			name,
			_elm_lang$core$Json_Decode$succeed(
				f(_debois$elm_mdl$Material_Ripple$Up)));
	});
var _debois$elm_mdl$Material_Ripple$upOn = _debois$elm_mdl$Material_Ripple$upOn_(_elm_lang$core$Basics$identity);
var _debois$elm_mdl$Material_Ripple$Down = function (a) {
	return {ctor: 'Down', _0: a};
};
var _debois$elm_mdl$Material_Ripple$downOn_ = F2(
	function (f, name) {
		return A2(
			_elm_lang$html$Html_Events$on,
			name,
			A2(
				_elm_lang$core$Json_Decode$map,
				function (_p6) {
					return f(
						_debois$elm_mdl$Material_Ripple$Down(_p6));
				},
				_debois$elm_mdl$Material_Ripple$geometryDecoder));
	});
var _debois$elm_mdl$Material_Ripple$downOn = _debois$elm_mdl$Material_Ripple$downOn_(_elm_lang$core$Basics$identity);
var _debois$elm_mdl$Material_Ripple$view = function (_p7) {
	return _debois$elm_mdl$Material_Ripple$view_(
		A3(
			_elm_lang$core$Basics$flip,
			_elm_lang$core$List$append,
			{
				ctor: '::',
				_0: _debois$elm_mdl$Material_Ripple$upOn('mouseup'),
				_1: {
					ctor: '::',
					_0: _debois$elm_mdl$Material_Ripple$upOn('mouseleave'),
					_1: {
						ctor: '::',
						_0: _debois$elm_mdl$Material_Ripple$upOn('touchend'),
						_1: {
							ctor: '::',
							_0: _debois$elm_mdl$Material_Ripple$upOn('blur'),
							_1: {
								ctor: '::',
								_0: _debois$elm_mdl$Material_Ripple$downOn('mousedown'),
								_1: {
									ctor: '::',
									_0: _debois$elm_mdl$Material_Ripple$downOn('touchstart'),
									_1: {ctor: '[]'}
								}
							}
						}
					}
				}
			},
			_p7));
};
var _debois$elm_mdl$Material_Ripple$down = F2(
	function (f, name) {
		return A2(
			_debois$elm_mdl$Material_Options$on,
			name,
			A2(
				_elm_lang$core$Json_Decode$map,
				function (_p8) {
					return f(
						_debois$elm_mdl$Material_Ripple$Down(_p8));
				},
				_debois$elm_mdl$Material_Ripple$geometryDecoder));
	});

var _debois$elm_mdl$Material_Button$_p0 = A3(
	_debois$elm_mdl$Material_Component$indexed,
	function (_) {
		return _.button;
	},
	F2(
		function (x, y) {
			return _elm_lang$core$Native_Utils.update(
				y,
				{button: x});
		}),
	_debois$elm_mdl$Material_Ripple$model);
var _debois$elm_mdl$Material_Button$get = _debois$elm_mdl$Material_Button$_p0._0;
var _debois$elm_mdl$Material_Button$set = _debois$elm_mdl$Material_Button$_p0._1;
var _debois$elm_mdl$Material_Button$icon = _debois$elm_mdl$Material_Options$cs('mdl-button--icon');
var _debois$elm_mdl$Material_Button$minifab = _debois$elm_mdl$Material_Options$cs('mdl-button--mini-fab');
var _debois$elm_mdl$Material_Button$fab = _debois$elm_mdl$Material_Options$cs('mdl-button--fab');
var _debois$elm_mdl$Material_Button$raised = _debois$elm_mdl$Material_Options$cs('mdl-button--raised');
var _debois$elm_mdl$Material_Button$flat = _debois$elm_mdl$Material_Options$nop;
var _debois$elm_mdl$Material_Button$blurAndForward = function (event) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		A2(_elm_lang$core$Basics_ops['++'], 'on', event),
		'this.blur(); (function(self) { var e = document.createEvent(\'Event\'); e.initEvent(\'touchcancel\', true, true); self.lastChild.dispatchEvent(e); }(this));');
};
var _debois$elm_mdl$Material_Button$type_ = function (_p1) {
	return _debois$elm_mdl$Material_Options_Internal$attribute(
		_elm_lang$html$Html_Attributes$type_(_p1));
};
var _debois$elm_mdl$Material_Button$accent = _debois$elm_mdl$Material_Options$cs('mdl-button--accent');
var _debois$elm_mdl$Material_Button$primary = _debois$elm_mdl$Material_Options$cs('mdl-button--primary');
var _debois$elm_mdl$Material_Button$colored = _debois$elm_mdl$Material_Options$cs('mdl-button--colored');
var _debois$elm_mdl$Material_Button$plain = _debois$elm_mdl$Material_Options$nop;
var _debois$elm_mdl$Material_Button$disabled = _debois$elm_mdl$Material_Options_Internal$attribute(
	_elm_lang$html$Html_Attributes$disabled(true));
var _debois$elm_mdl$Material_Button$ripple = _debois$elm_mdl$Material_Options_Internal$option(
	function (options) {
		return _elm_lang$core$Native_Utils.update(
			options,
			{ripple: true});
	});
var _debois$elm_mdl$Material_Button$link = function (href) {
	return _debois$elm_mdl$Material_Options$many(
		{
			ctor: '::',
			_0: _debois$elm_mdl$Material_Options_Internal$option(
				function (options) {
					return _elm_lang$core$Native_Utils.update(
						options,
						{link: true});
				}),
			_1: {
				ctor: '::',
				_0: _debois$elm_mdl$Material_Options_Internal$attribute(
					_elm_lang$html$Html_Attributes$href(href)),
				_1: {ctor: '[]'}
			}
		});
};
var _debois$elm_mdl$Material_Button$defaultConfig = {ripple: false, link: false};
var _debois$elm_mdl$Material_Button$view = F4(
	function (lift, model, config, html) {
		var listeners = _debois$elm_mdl$Material_Options$many(
			{
				ctor: '::',
				_0: A2(_debois$elm_mdl$Material_Ripple$down, lift, 'mousedown'),
				_1: {
					ctor: '::',
					_0: A2(_debois$elm_mdl$Material_Ripple$down, lift, 'touchstart'),
					_1: {
						ctor: '::',
						_0: A2(_debois$elm_mdl$Material_Ripple$up, lift, 'touchcancel'),
						_1: {
							ctor: '::',
							_0: A2(_debois$elm_mdl$Material_Ripple$up, lift, 'mouseup'),
							_1: {
								ctor: '::',
								_0: A2(_debois$elm_mdl$Material_Ripple$up, lift, 'blur'),
								_1: {
									ctor: '::',
									_0: A2(_debois$elm_mdl$Material_Ripple$up, lift, 'mouseleave'),
									_1: {ctor: '[]'}
								}
							}
						}
					}
				}
			});
		var summary = A2(_debois$elm_mdl$Material_Options_Internal$collect, _debois$elm_mdl$Material_Button$defaultConfig, config);
		return A5(
			_debois$elm_mdl$Material_Options_Internal$apply,
			summary,
			summary.config.link ? _elm_lang$html$Html$a : _elm_lang$html$Html$button,
			{
				ctor: '::',
				_0: _debois$elm_mdl$Material_Options$cs('mdl-button'),
				_1: {
					ctor: '::',
					_0: _debois$elm_mdl$Material_Options$cs('mdl-js-button'),
					_1: {
						ctor: '::',
						_0: A2(
							_debois$elm_mdl$Material_Options$when,
							summary.config.ripple,
							_debois$elm_mdl$Material_Options$cs('mdl-js-ripple-effect')),
						_1: {
							ctor: '::',
							_0: listeners,
							_1: {ctor: '[]'}
						}
					}
				}
			},
			{
				ctor: '::',
				_0: _debois$elm_mdl$Material_Helpers$blurOn('mouseup'),
				_1: {
					ctor: '::',
					_0: _debois$elm_mdl$Material_Helpers$blurOn('mouseleave'),
					_1: {
						ctor: '::',
						_0: _debois$elm_mdl$Material_Helpers$blurOn('touchend'),
						_1: {ctor: '[]'}
					}
				}
			},
			summary.config.ripple ? _elm_lang$core$List$concat(
				{
					ctor: '::',
					_0: html,
					_1: {
						ctor: '::',
						_0: {
							ctor: '::',
							_0: A2(
								_elm_lang$html$Html$map,
								lift,
								A2(
									_debois$elm_mdl$Material_Ripple$view_,
									{
										ctor: '::',
										_0: _elm_lang$html$Html_Attributes$class('mdl-button__ripple-container'),
										_1: {ctor: '[]'}
									},
									model)),
							_1: {ctor: '[]'}
						},
						_1: {ctor: '[]'}
					}
				}) : html);
	});
var _debois$elm_mdl$Material_Button$render = A3(_debois$elm_mdl$Material_Component$render, _debois$elm_mdl$Material_Button$get, _debois$elm_mdl$Material_Button$view, _debois$elm_mdl$Material_Component$ButtonMsg);
var _debois$elm_mdl$Material_Button$update = function (action) {
	return _debois$elm_mdl$Material_Ripple$update(action);
};
var _debois$elm_mdl$Material_Button$react = A4(
	_debois$elm_mdl$Material_Component$react,
	_debois$elm_mdl$Material_Button$get,
	_debois$elm_mdl$Material_Button$set,
	_debois$elm_mdl$Material_Component$ButtonMsg,
	_debois$elm_mdl$Material_Component$generalise(_debois$elm_mdl$Material_Button$update));
var _debois$elm_mdl$Material_Button$defaultModel = _debois$elm_mdl$Material_Ripple$model;
var _debois$elm_mdl$Material_Button$Config = F2(
	function (a, b) {
		return {ripple: a, link: b};
	});

var _debois$elm_mdl$Material_Icon$view = F2(
	function (name, options) {
		return A3(
			_debois$elm_mdl$Material_Options$styled,
			_elm_lang$html$Html$i,
			{
				ctor: '::',
				_0: _debois$elm_mdl$Material_Options$cs('material-icons'),
				_1: options
			},
			{
				ctor: '::',
				_0: _elm_lang$html$Html$text(name),
				_1: {ctor: '[]'}
			});
	});
var _debois$elm_mdl$Material_Icon$i = function (name) {
	return A2(
		_debois$elm_mdl$Material_Icon$view,
		name,
		{ctor: '[]'});
};
var _debois$elm_mdl$Material_Icon$size48 = A2(_debois$elm_mdl$Material_Options$css, 'font-size', '48px');
var _debois$elm_mdl$Material_Icon$size36 = A2(_debois$elm_mdl$Material_Options$css, 'font-size', '36px');
var _debois$elm_mdl$Material_Icon$size24 = A2(_debois$elm_mdl$Material_Options$css, 'font-size', '24px');
var _debois$elm_mdl$Material_Icon$size18 = A2(_debois$elm_mdl$Material_Options$css, 'font-size', '18px');
var _debois$elm_mdl$Material_Icon$defaultConfig = {};
var _debois$elm_mdl$Material_Icon$Config = {};

var _debois$elm_mdl$Material_Textfield$update = F3(
	function (_p0, action, model) {
		return A3(
			_elm_lang$core$Basics$flip,
			F2(
				function (x, y) {
					return A2(_elm_lang$core$Platform_Cmd_ops['!'], x, y);
				}),
			{ctor: '[]'},
			function () {
				var _p1 = action;
				switch (_p1.ctor) {
					case 'Input':
						var dirty = !_elm_lang$core$Native_Utils.eq(_p1._0, '');
						return _elm_lang$core$Native_Utils.eq(dirty, model.isDirty) ? _elm_lang$core$Maybe$Nothing : _elm_lang$core$Maybe$Just(
							_elm_lang$core$Native_Utils.update(
								model,
								{isDirty: dirty}));
					case 'Blur':
						return _elm_lang$core$Maybe$Just(
							_elm_lang$core$Native_Utils.update(
								model,
								{isFocused: false}));
					default:
						return _elm_lang$core$Maybe$Just(
							_elm_lang$core$Native_Utils.update(
								model,
								{isFocused: true}));
				}
			}());
	});
var _debois$elm_mdl$Material_Textfield$defaultModel = {isFocused: false, isDirty: false};
var _debois$elm_mdl$Material_Textfield$_p2 = A3(
	_debois$elm_mdl$Material_Component$indexed,
	function (_) {
		return _.textfield;
	},
	F2(
		function (x, c) {
			return _elm_lang$core$Native_Utils.update(
				c,
				{textfield: x});
		}),
	_debois$elm_mdl$Material_Textfield$defaultModel);
var _debois$elm_mdl$Material_Textfield$get = _debois$elm_mdl$Material_Textfield$_p2._0;
var _debois$elm_mdl$Material_Textfield$set = _debois$elm_mdl$Material_Textfield$_p2._1;
var _debois$elm_mdl$Material_Textfield$react = A4(_debois$elm_mdl$Material_Component$react, _debois$elm_mdl$Material_Textfield$get, _debois$elm_mdl$Material_Textfield$set, _debois$elm_mdl$Material_Component$TextfieldMsg, _debois$elm_mdl$Material_Textfield$update);
var _debois$elm_mdl$Material_Textfield$cols = function (k) {
	return _debois$elm_mdl$Material_Options_Internal$input(
		{
			ctor: '::',
			_0: _debois$elm_mdl$Material_Options$attribute(
				_elm_lang$html$Html_Attributes$cols(k)),
			_1: {ctor: '[]'}
		});
};
var _debois$elm_mdl$Material_Textfield$rows = function (k) {
	return _debois$elm_mdl$Material_Options_Internal$input(
		{
			ctor: '::',
			_0: _debois$elm_mdl$Material_Options$attribute(
				_elm_lang$html$Html_Attributes$rows(k)),
			_1: {ctor: '[]'}
		});
};
var _debois$elm_mdl$Material_Textfield$input = _debois$elm_mdl$Material_Options$input;
var _debois$elm_mdl$Material_Textfield$disabled = _debois$elm_mdl$Material_Options_Internal$option(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{disabled: true});
	});
var _debois$elm_mdl$Material_Textfield$maxlength = function (k) {
	return _debois$elm_mdl$Material_Options$attribute(
		_elm_lang$html$Html_Attributes$maxlength(k));
};
var _debois$elm_mdl$Material_Textfield$autofocus = _debois$elm_mdl$Material_Options$attribute(
	_elm_lang$html$Html_Attributes$autofocus(true));
var _debois$elm_mdl$Material_Textfield$value = function (_p3) {
	return _debois$elm_mdl$Material_Options_Internal$option(
		F2(
			function (str, config) {
				return _elm_lang$core$Native_Utils.update(
					config,
					{
						value: _elm_lang$core$Maybe$Just(str)
					});
			})(_p3));
};
var _debois$elm_mdl$Material_Textfield$error = function (_p4) {
	return _debois$elm_mdl$Material_Options_Internal$option(
		F2(
			function (str, config) {
				return _elm_lang$core$Native_Utils.update(
					config,
					{
						error: _elm_lang$core$Maybe$Just(str)
					});
			})(_p4));
};
var _debois$elm_mdl$Material_Textfield$expandableIcon = function (id) {
	return _debois$elm_mdl$Material_Options_Internal$option(
		function (config) {
			return _elm_lang$core$Native_Utils.update(
				config,
				{expandableIcon: id});
		});
};
var _debois$elm_mdl$Material_Textfield$expandable = function (id) {
	return _debois$elm_mdl$Material_Options_Internal$option(
		function (config) {
			return _elm_lang$core$Native_Utils.update(
				config,
				{
					expandable: _elm_lang$core$Maybe$Just(id)
				});
		});
};
var _debois$elm_mdl$Material_Textfield$floatingLabel = _debois$elm_mdl$Material_Options_Internal$option(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{labelFloat: true});
	});
var _debois$elm_mdl$Material_Textfield$label = function (_p5) {
	return _debois$elm_mdl$Material_Options_Internal$option(
		F2(
			function (str, config) {
				return _elm_lang$core$Native_Utils.update(
					config,
					{
						labelText: _elm_lang$core$Maybe$Just(str)
					});
			})(_p5));
};
var _debois$elm_mdl$Material_Textfield$Config = function (a) {
	return function (b) {
		return function (c) {
			return function (d) {
				return function (e) {
					return function (f) {
						return function (g) {
							return function (h) {
								return function (i) {
									return function (j) {
										return {labelText: a, labelFloat: b, error: c, value: d, disabled: e, kind: f, expandable: g, expandableIcon: h, input: i, container: j};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var _debois$elm_mdl$Material_Textfield$Model = F2(
	function (a, b) {
		return {isFocused: a, isDirty: b};
	});
var _debois$elm_mdl$Material_Textfield$Email = {ctor: 'Email'};
var _debois$elm_mdl$Material_Textfield$email = _debois$elm_mdl$Material_Options_Internal$option(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{kind: _debois$elm_mdl$Material_Textfield$Email});
	});
var _debois$elm_mdl$Material_Textfield$Password = {ctor: 'Password'};
var _debois$elm_mdl$Material_Textfield$password = _debois$elm_mdl$Material_Options_Internal$option(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{kind: _debois$elm_mdl$Material_Textfield$Password});
	});
var _debois$elm_mdl$Material_Textfield$Textarea = {ctor: 'Textarea'};
var _debois$elm_mdl$Material_Textfield$textarea = _debois$elm_mdl$Material_Options_Internal$option(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{kind: _debois$elm_mdl$Material_Textfield$Textarea});
	});
var _debois$elm_mdl$Material_Textfield$Text = {ctor: 'Text'};
var _debois$elm_mdl$Material_Textfield$defaultConfig = {
	labelText: _elm_lang$core$Maybe$Nothing,
	labelFloat: false,
	error: _elm_lang$core$Maybe$Nothing,
	value: _elm_lang$core$Maybe$Nothing,
	disabled: false,
	kind: _debois$elm_mdl$Material_Textfield$Text,
	expandable: _elm_lang$core$Maybe$Nothing,
	expandableIcon: 'search',
	input: {ctor: '[]'},
	container: {ctor: '[]'}
};
var _debois$elm_mdl$Material_Textfield$text_ = _debois$elm_mdl$Material_Options_Internal$option(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{kind: _debois$elm_mdl$Material_Textfield$Text});
	});
var _debois$elm_mdl$Material_Textfield$Input = function (a) {
	return {ctor: 'Input', _0: a};
};
var _debois$elm_mdl$Material_Textfield$Focus = {ctor: 'Focus'};
var _debois$elm_mdl$Material_Textfield$Blur = {ctor: 'Blur'};
var _debois$elm_mdl$Material_Textfield$view = F4(
	function (lift, model, options, _p6) {
		var _p7 = A2(_debois$elm_mdl$Material_Options_Internal$collect, _debois$elm_mdl$Material_Textfield$defaultConfig, options);
		var summary = _p7;
		var config = _p7.config;
		var labelFor = function () {
			var _p8 = config.expandable;
			if (_p8.ctor === 'Nothing') {
				return {ctor: '[]'};
			} else {
				return {
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$for(_p8._0),
					_1: {ctor: '[]'}
				};
			}
		}();
		var expandableId = function () {
			var _p9 = config.expandable;
			if (_p9.ctor === 'Nothing') {
				return _debois$elm_mdl$Material_Options$nop;
			} else {
				return _debois$elm_mdl$Material_Options_Internal$attribute(
					_elm_lang$html$Html_Attributes$id(_p9._0));
			}
		}();
		var expHolder = function () {
			var _p10 = config.expandable;
			if (_p10.ctor === 'Nothing') {
				return _elm_lang$core$Basics$identity;
			} else {
				return function (x) {
					return {
						ctor: '::',
						_0: A4(
							_debois$elm_mdl$Material_Options$styled_,
							_elm_lang$html$Html$label,
							{
								ctor: '::',
								_0: _debois$elm_mdl$Material_Options$cs('mdl-button'),
								_1: {
									ctor: '::',
									_0: _debois$elm_mdl$Material_Options$cs('mdl-js-button'),
									_1: {
										ctor: '::',
										_0: _debois$elm_mdl$Material_Options$cs('mdl-button--icon'),
										_1: {ctor: '[]'}
									}
								}
							},
							labelFor,
							{
								ctor: '::',
								_0: _debois$elm_mdl$Material_Icon$i(config.expandableIcon),
								_1: {ctor: '[]'}
							}),
						_1: {
							ctor: '::',
							_0: A3(
								_debois$elm_mdl$Material_Options$styled,
								_elm_lang$html$Html$div,
								{
									ctor: '::',
									_0: _debois$elm_mdl$Material_Options$cs('mdl-textfield__expandable-holder'),
									_1: {ctor: '[]'}
								},
								x),
							_1: {ctor: '[]'}
						}
					};
				};
			}
		}();
		return A4(
			_debois$elm_mdl$Material_Options_Internal$applyContainer,
			summary,
			_elm_lang$html$Html$div,
			{
				ctor: '::',
				_0: _debois$elm_mdl$Material_Options$cs('mdl-textfield'),
				_1: {
					ctor: '::',
					_0: _debois$elm_mdl$Material_Options$cs('mdl-js-textfield'),
					_1: {
						ctor: '::',
						_0: _debois$elm_mdl$Material_Options$cs('is-upgraded'),
						_1: {
							ctor: '::',
							_0: A3(_debois$elm_mdl$Material_Options_Internal$on1, 'focus', lift, _debois$elm_mdl$Material_Textfield$Focus),
							_1: {
								ctor: '::',
								_0: A3(_debois$elm_mdl$Material_Options_Internal$on1, 'blur', lift, _debois$elm_mdl$Material_Textfield$Blur),
								_1: {
									ctor: '::',
									_0: A2(
										_debois$elm_mdl$Material_Options$when,
										config.labelFloat,
										_debois$elm_mdl$Material_Options$cs('mdl-textfield--floating-label')),
									_1: {
										ctor: '::',
										_0: A2(
											_debois$elm_mdl$Material_Options$when,
											!_elm_lang$core$Native_Utils.eq(config.error, _elm_lang$core$Maybe$Nothing),
											_debois$elm_mdl$Material_Options$cs('is-invalid')),
										_1: {
											ctor: '::',
											_0: A2(
												_debois$elm_mdl$Material_Options$when,
												function () {
													var _p11 = config.value;
													if (_p11.ctor === 'Just') {
														if (_p11._0 === '') {
															return false;
														} else {
															return true;
														}
													} else {
														return model.isDirty;
													}
												}(),
												_debois$elm_mdl$Material_Options$cs('is-dirty')),
											_1: {
												ctor: '::',
												_0: A2(
													_debois$elm_mdl$Material_Options$when,
													model.isFocused && (!config.disabled),
													_debois$elm_mdl$Material_Options$cs('is-focused')),
												_1: {
													ctor: '::',
													_0: A2(
														_debois$elm_mdl$Material_Options$when,
														config.disabled,
														_debois$elm_mdl$Material_Options$cs('is-disabled')),
													_1: {
														ctor: '::',
														_0: A2(
															_debois$elm_mdl$Material_Options$when,
															!_elm_lang$core$Native_Utils.eq(config.expandable, _elm_lang$core$Maybe$Nothing),
															_debois$elm_mdl$Material_Options$cs('mdl-textfield--expandable')),
														_1: {ctor: '[]'}
													}
												}
											}
										}
									}
								}
							}
						}
					}
				}
			},
			expHolder(
				{
					ctor: '::',
					_0: A4(
						_debois$elm_mdl$Material_Options_Internal$applyInput,
						summary,
						_elm_lang$core$Native_Utils.eq(config.kind, _debois$elm_mdl$Material_Textfield$Textarea) ? _elm_lang$html$Html$textarea : _elm_lang$html$Html$input,
						{
							ctor: '::',
							_0: _debois$elm_mdl$Material_Options$cs('mdl-textfield__input'),
							_1: {
								ctor: '::',
								_0: A2(_debois$elm_mdl$Material_Options$css, 'outline', 'none'),
								_1: {
									ctor: '::',
									_0: A3(_debois$elm_mdl$Material_Options_Internal$on1, 'focus', lift, _debois$elm_mdl$Material_Textfield$Focus),
									_1: {
										ctor: '::',
										_0: A3(_debois$elm_mdl$Material_Options_Internal$on1, 'blur', lift, _debois$elm_mdl$Material_Textfield$Blur),
										_1: {
											ctor: '::',
											_0: function () {
												var _p12 = config.kind;
												switch (_p12.ctor) {
													case 'Text':
														return _debois$elm_mdl$Material_Options_Internal$attribute(
															_elm_lang$html$Html_Attributes$type_('text'));
													case 'Password':
														return _debois$elm_mdl$Material_Options_Internal$attribute(
															_elm_lang$html$Html_Attributes$type_('password'));
													case 'Email':
														return _debois$elm_mdl$Material_Options_Internal$attribute(
															_elm_lang$html$Html_Attributes$type_('email'));
													default:
														return _debois$elm_mdl$Material_Options$nop;
												}
											}(),
											_1: {
												ctor: '::',
												_0: A2(
													_debois$elm_mdl$Material_Options$when,
													config.disabled,
													_debois$elm_mdl$Material_Options_Internal$attribute(
														_elm_lang$html$Html_Attributes$disabled(true))),
												_1: {
													ctor: '::',
													_0: expandableId,
													_1: {
														ctor: '::',
														_0: function () {
															var _p13 = config.value;
															if (_p13.ctor === 'Nothing') {
																return A2(
																	_debois$elm_mdl$Material_Options$on,
																	'input',
																	A2(
																		_elm_lang$core$Json_Decode$map,
																		function (_p14) {
																			return lift(
																				_debois$elm_mdl$Material_Textfield$Input(_p14));
																		},
																		_elm_lang$html$Html_Events$targetValue));
															} else {
																return _debois$elm_mdl$Material_Options_Internal$attribute(
																	_elm_lang$html$Html_Attributes$value(_p13._0));
															}
														}(),
														_1: {ctor: '[]'}
													}
												}
											}
										}
									}
								}
							}
						},
						{ctor: '[]'}),
					_1: {
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$label,
							A2(
								_elm_lang$core$Basics_ops['++'],
								{
									ctor: '::',
									_0: _elm_lang$html$Html_Attributes$class('mdl-textfield__label'),
									_1: {ctor: '[]'}
								},
								labelFor),
							function () {
								var _p15 = config.labelText;
								if (_p15.ctor === 'Just') {
									return {
										ctor: '::',
										_0: _elm_lang$html$Html$text(_p15._0),
										_1: {ctor: '[]'}
									};
								} else {
									return {ctor: '[]'};
								}
							}()),
						_1: {
							ctor: '::',
							_0: A2(
								_elm_lang$core$Maybe$withDefault,
								A2(
									_elm_lang$html$Html$div,
									{ctor: '[]'},
									{ctor: '[]'}),
								A2(
									_elm_lang$core$Maybe$map,
									function (e) {
										return A2(
											_elm_lang$html$Html$span,
											{
												ctor: '::',
												_0: _elm_lang$html$Html_Attributes$class('mdl-textfield__error'),
												_1: {ctor: '[]'}
											},
											{
												ctor: '::',
												_0: _elm_lang$html$Html$text(e),
												_1: {ctor: '[]'}
											});
									},
									config.error)),
							_1: {ctor: '[]'}
						}
					}
				}));
	});
var _debois$elm_mdl$Material_Textfield$render = A3(_debois$elm_mdl$Material_Component$render, _debois$elm_mdl$Material_Textfield$get, _debois$elm_mdl$Material_Textfield$view, _debois$elm_mdl$Material_Component$TextfieldMsg);

var _debois$elm_mdl$Material_Menu_Geometry$Geometry = F5(
	function (a, b, c, d, e) {
		return {button: a, menu: b, container: c, offsetTops: d, offsetHeights: e};
	});
var _debois$elm_mdl$Material_Menu_Geometry$Element = F4(
	function (a, b, c, d) {
		return {offsetTop: a, offsetLeft: b, offsetHeight: c, bounds: d};
	});
var _debois$elm_mdl$Material_Menu_Geometry$element = A5(_elm_lang$core$Json_Decode$map4, _debois$elm_mdl$Material_Menu_Geometry$Element, _debois$elm_dom$DOM$offsetTop, _debois$elm_dom$DOM$offsetLeft, _debois$elm_dom$DOM$offsetHeight, _debois$elm_dom$DOM$boundingClientRect);
var _debois$elm_mdl$Material_Menu_Geometry$decode = A6(
	_elm_lang$core$Json_Decode$map5,
	_debois$elm_mdl$Material_Menu_Geometry$Geometry,
	_debois$elm_dom$DOM$target(_debois$elm_mdl$Material_Menu_Geometry$element),
	_debois$elm_dom$DOM$target(
		_debois$elm_dom$DOM$nextSibling(
			A2(_debois$elm_dom$DOM$childNode, 1, _debois$elm_mdl$Material_Menu_Geometry$element))),
	_debois$elm_dom$DOM$target(
		_debois$elm_dom$DOM$nextSibling(_debois$elm_mdl$Material_Menu_Geometry$element)),
	_debois$elm_dom$DOM$target(
		_debois$elm_dom$DOM$nextSibling(
			A2(
				_debois$elm_dom$DOM$childNode,
				1,
				_debois$elm_dom$DOM$childNodes(_debois$elm_dom$DOM$offsetTop)))),
	_debois$elm_dom$DOM$target(
		_debois$elm_dom$DOM$nextSibling(
			A2(
				_debois$elm_dom$DOM$childNode,
				1,
				_debois$elm_dom$DOM$childNodes(_debois$elm_dom$DOM$offsetHeight)))));

var _debois$elm_mdl$Material_Menu$toPx = function (_p0) {
	return A3(
		_elm_lang$core$Basics$flip,
		F2(
			function (x, y) {
				return A2(_elm_lang$core$Basics_ops['++'], x, y);
			}),
		'px',
		_elm_lang$core$Basics$toString(_p0));
};
var _debois$elm_mdl$Material_Menu$rect = F4(
	function (x, y, w, h) {
		return function (coords) {
			return A2(
				_elm_lang$core$Basics_ops['++'],
				'rect(',
				A2(_elm_lang$core$Basics_ops['++'], coords, ')'));
		}(
			A2(
				_elm_lang$core$String$join,
				' ',
				A2(
					_elm_lang$core$List$map,
					_debois$elm_mdl$Material_Menu$toPx,
					{
						ctor: '::',
						_0: x,
						_1: {
							ctor: '::',
							_0: y,
							_1: {
								ctor: '::',
								_0: w,
								_1: {
									ctor: '::',
									_0: h,
									_1: {ctor: '[]'}
								}
							}
						}
					})));
	});
var _debois$elm_mdl$Material_Menu$onKeyDown = function (action) {
	return A3(
		_debois$elm_mdl$Material_Options$onWithOptions,
		'keydown',
		{preventDefault: true, stopPropagation: false},
		A2(_elm_lang$core$Json_Decode$map, action, _elm_lang$html$Html_Events$keyCode));
};
var _debois$elm_mdl$Material_Menu$onClick = F2(
	function (decoder, action) {
		return A2(
			_elm_lang$html$Html_Events$on,
			'click',
			A2(_elm_lang$core$Json_Decode$map, action, decoder));
	});
var _debois$elm_mdl$Material_Menu$withGeometry = F2(
	function (model, f) {
		return A2(
			_elm_lang$core$Maybe$withDefault,
			_debois$elm_mdl$Material_Options$nop,
			A2(_elm_lang$core$Maybe$map, f, model.geometry));
	});
var _debois$elm_mdl$Material_Menu$icon = function (_p1) {
	return _debois$elm_mdl$Material_Options_Internal$option(
		F2(
			function (name, config) {
				return _elm_lang$core$Native_Utils.update(
					config,
					{icon: name});
			})(_p1));
};
var _debois$elm_mdl$Material_Menu$ripple = _debois$elm_mdl$Material_Options_Internal$option(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{ripple: true});
	});
var _debois$elm_mdl$Material_Menu$onSelect = function (_p2) {
	return _debois$elm_mdl$Material_Options_Internal$option(
		F2(
			function (msg, config) {
				return _elm_lang$core$Native_Utils.update(
					config,
					{
						onSelect: _elm_lang$core$Maybe$Just(msg)
					});
			})(_p2));
};
var _debois$elm_mdl$Material_Menu$disabled = _debois$elm_mdl$Material_Options_Internal$option(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{enabled: false});
	});
var _debois$elm_mdl$Material_Menu$divider = _debois$elm_mdl$Material_Options_Internal$option(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{divider: true});
	});
var _debois$elm_mdl$Material_Menu$defaultItemConfig = {enabled: true, divider: false, onSelect: _elm_lang$core$Maybe$Nothing};
var _debois$elm_mdl$Material_Menu$constant = {transitionDurationSeconds: 0.3, transitionDurationFraction: 0.8, closeTimeout: 150};
var _debois$elm_mdl$Material_Menu$transitionDuration = _debois$elm_mdl$Material_Menu$constant.transitionDurationSeconds * _debois$elm_mdl$Material_Menu$constant.transitionDurationFraction;
var _debois$elm_mdl$Material_Menu$Model = F4(
	function (a, b, c, d) {
		return {ripples: a, animationState: b, geometry: c, index: d};
	});
var _debois$elm_mdl$Material_Menu$Item = F2(
	function (a, b) {
		return {options: a, html: b};
	});
var _debois$elm_mdl$Material_Menu$item = _debois$elm_mdl$Material_Menu$Item;
var _debois$elm_mdl$Material_Menu$ItemConfig = F3(
	function (a, b, c) {
		return {enabled: a, divider: b, onSelect: c};
	});
var _debois$elm_mdl$Material_Menu$Config = F3(
	function (a, b, c) {
		return {alignment: a, ripple: b, icon: c};
	});
var _debois$elm_mdl$Material_Menu$Closing = {ctor: 'Closing'};
var _debois$elm_mdl$Material_Menu$Opened = {ctor: 'Opened'};
var _debois$elm_mdl$Material_Menu$clip = F3(
	function (model, config, geometry) {
		var height = geometry.menu.bounds.height;
		var width = geometry.menu.bounds.width;
		return A2(
			_debois$elm_mdl$Material_Options$css,
			'clip',
			function () {
				if (_elm_lang$core$Native_Utils.eq(model.animationState, _debois$elm_mdl$Material_Menu$Opened) || _elm_lang$core$Native_Utils.eq(model.animationState, _debois$elm_mdl$Material_Menu$Closing)) {
					return A4(_debois$elm_mdl$Material_Menu$rect, 0, width, height, 0);
				} else {
					var _p3 = config.alignment;
					switch (_p3.ctor) {
						case 'BottomRight':
							return A4(_debois$elm_mdl$Material_Menu$rect, 0, width, 0, width);
						case 'TopLeft':
							return A4(_debois$elm_mdl$Material_Menu$rect, height, 0, height, 0);
						case 'TopRight':
							return A4(_debois$elm_mdl$Material_Menu$rect, height, width, height, width);
						default:
							return '';
					}
				}
			}());
	});
var _debois$elm_mdl$Material_Menu$Opening = {ctor: 'Opening'};
var _debois$elm_mdl$Material_Menu$isActive = function (model) {
	return _elm_lang$core$Native_Utils.eq(model.animationState, _debois$elm_mdl$Material_Menu$Opened) || _elm_lang$core$Native_Utils.eq(model.animationState, _debois$elm_mdl$Material_Menu$Opening);
};
var _debois$elm_mdl$Material_Menu$Idle = {ctor: 'Idle'};
var _debois$elm_mdl$Material_Menu$defaultModel = {ripples: _elm_lang$core$Dict$empty, animationState: _debois$elm_mdl$Material_Menu$Idle, geometry: _elm_lang$core$Maybe$Nothing, index: _elm_lang$core$Maybe$Nothing};
var _debois$elm_mdl$Material_Menu$_p4 = A3(
	_debois$elm_mdl$Material_Component$indexed,
	function (_) {
		return _.menu;
	},
	F2(
		function (x, y) {
			return _elm_lang$core$Native_Utils.update(
				y,
				{menu: x});
		}),
	_debois$elm_mdl$Material_Menu$defaultModel);
var _debois$elm_mdl$Material_Menu$get = _debois$elm_mdl$Material_Menu$_p4._0;
var _debois$elm_mdl$Material_Menu$set = _debois$elm_mdl$Material_Menu$_p4._1;
var _debois$elm_mdl$Material_Menu$Key = F2(
	function (a, b) {
		return {ctor: 'Key', _0: a, _1: b};
	});
var _debois$elm_mdl$Material_Menu$Click = function (a) {
	return {ctor: 'Click', _0: a};
};
var _debois$elm_mdl$Material_Menu$subscriptions = function (model) {
	return _elm_lang$core$Native_Utils.eq(model.animationState, _debois$elm_mdl$Material_Menu$Opened) ? _elm_lang$mouse$Mouse$clicks(_debois$elm_mdl$Material_Menu$Click) : _elm_lang$core$Platform_Sub$none;
};
var _debois$elm_mdl$Material_Menu$subs = A3(
	_debois$elm_mdl$Material_Component$subs,
	_debois$elm_mdl$Material_Component$MenuMsg,
	function (_) {
		return _.menu;
	},
	_debois$elm_mdl$Material_Menu$subscriptions);
var _debois$elm_mdl$Material_Menu$Ripple = F2(
	function (a, b) {
		return {ctor: 'Ripple', _0: a, _1: b};
	});
var _debois$elm_mdl$Material_Menu$Tick = {ctor: 'Tick'};
var _debois$elm_mdl$Material_Menu$Close = {ctor: 'Close'};
var _debois$elm_mdl$Material_Menu$Select = F2(
	function (a, b) {
		return {ctor: 'Select', _0: a, _1: b};
	});
var _debois$elm_mdl$Material_Menu$update = F3(
	function (fwd, msg, model) {
		update:
		while (true) {
			var _p5 = msg;
			switch (_p5.ctor) {
				case 'Open':
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model,
							{
								animationState: function () {
									var _p6 = model.animationState;
									if (_p6.ctor === 'Opened') {
										return _debois$elm_mdl$Material_Menu$Opened;
									} else {
										return _debois$elm_mdl$Material_Menu$Opening;
									}
								}(),
								geometry: _elm_lang$core$Maybe$Just(_p5._0)
							}),
						_1: _debois$elm_mdl$Material_Helpers$cmd(
							fwd(_debois$elm_mdl$Material_Menu$Tick))
					};
				case 'Tick':
					return _debois$elm_mdl$Material_Helpers$pure(
						_elm_lang$core$Native_Utils.update(
							model,
							{animationState: _debois$elm_mdl$Material_Menu$Opened}));
				case 'Close':
					return _debois$elm_mdl$Material_Helpers$pure(
						_elm_lang$core$Native_Utils.update(
							model,
							{animationState: _debois$elm_mdl$Material_Menu$Idle, geometry: _elm_lang$core$Maybe$Nothing, index: _elm_lang$core$Maybe$Nothing}));
				case 'Select':
					var cmds = A2(
						_elm_lang$core$List$filterMap,
						_elm_lang$core$Basics$identity,
						{
							ctor: '::',
							_0: _elm_lang$core$Maybe$Just(
								A2(
									_debois$elm_mdl$Material_Helpers$delay,
									_debois$elm_mdl$Material_Menu$constant.closeTimeout,
									fwd(_debois$elm_mdl$Material_Menu$Close))),
							_1: {
								ctor: '::',
								_0: A2(_elm_lang$core$Maybe$map, _debois$elm_mdl$Material_Helpers$cmd, _p5._1),
								_1: {ctor: '[]'}
							}
						});
					var model_ = _elm_lang$core$Native_Utils.update(
						model,
						{animationState: _debois$elm_mdl$Material_Menu$Closing});
					return {
						ctor: '_Tuple2',
						_0: model_,
						_1: _elm_lang$core$Platform_Cmd$batch(cmds)
					};
				case 'Ripple':
					var _p9 = _p5._0;
					var _p7 = A2(
						_debois$elm_mdl$Material_Ripple$update,
						_p5._1,
						A2(
							_elm_lang$core$Maybe$withDefault,
							_debois$elm_mdl$Material_Ripple$model,
							A2(_elm_lang$core$Dict$get, _p9, model.ripples)));
					var model_ = _p7._0;
					var effects = _p7._1;
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model,
							{
								ripples: A3(_elm_lang$core$Dict$insert, _p9, model_, model.ripples)
							}),
						_1: A2(
							_elm_lang$core$Platform_Cmd$map,
							function (_p8) {
								return fwd(
									A2(_debois$elm_mdl$Material_Menu$Ripple, _p9, _p8));
							},
							effects)
					};
				case 'Click':
					if (_debois$elm_mdl$Material_Menu$isActive(model)) {
						var _p10 = model.geometry;
						if (_p10.ctor === 'Just') {
							var inside = F2(
								function (_p12, _p11) {
									var _p13 = _p12;
									var _p18 = _p13.y;
									var _p17 = _p13.x;
									var _p14 = _p11;
									var _p16 = _p14.top;
									var _p15 = _p14.left;
									return (_elm_lang$core$Native_Utils.cmp(
										_p15,
										_elm_lang$core$Basics$toFloat(_p17)) < 1) && ((_elm_lang$core$Native_Utils.cmp(
										_elm_lang$core$Basics$toFloat(_p17),
										_p15 + _p14.width) < 1) && ((_elm_lang$core$Native_Utils.cmp(
										_p16,
										_elm_lang$core$Basics$toFloat(_p18)) < 1) && (_elm_lang$core$Native_Utils.cmp(
										_elm_lang$core$Basics$toFloat(_p18),
										_p16 + _p14.height) < 1)));
								});
							if (A2(inside, _p5._0, _p10._0.menu.bounds)) {
								return A2(
									_elm_lang$core$Platform_Cmd_ops['!'],
									model,
									{ctor: '[]'});
							} else {
								var _v6 = fwd,
									_v7 = _debois$elm_mdl$Material_Menu$Close,
									_v8 = model;
								fwd = _v6;
								msg = _v7;
								model = _v8;
								continue update;
							}
						} else {
							return A2(
								_elm_lang$core$Platform_Cmd_ops['!'],
								model,
								{ctor: '[]'});
						}
					} else {
						return A2(
							_elm_lang$core$Platform_Cmd_ops['!'],
							model,
							{ctor: '[]'});
					}
				default:
					var _p27 = _p5._0;
					var _p19 = _p5._1;
					switch (_p19) {
						case 13:
							if (_debois$elm_mdl$Material_Menu$isActive(model)) {
								var _p20 = model.index;
								if (_p20.ctor === 'Just') {
									var _p22 = _p20._0;
									var cmd = A2(
										_elm_lang$core$Maybe$andThen,
										function (_p21) {
											return function (_) {
												return _.onSelect;
											}(
												function (_) {
													return _.config;
												}(_p21));
										},
										_elm_lang$core$List$head(
											A2(_elm_lang$core$List$drop, _p22, _p27)));
									var _v11 = fwd,
										_v12 = A2(_debois$elm_mdl$Material_Menu$Select, _p22 + 1, cmd),
										_v13 = model;
									fwd = _v11;
									msg = _v12;
									model = _v13;
									continue update;
								} else {
									var _v14 = fwd,
										_v15 = _debois$elm_mdl$Material_Menu$Close,
										_v16 = model;
									fwd = _v14;
									msg = _v15;
									model = _v16;
									continue update;
								}
							} else {
								return A2(
									_elm_lang$core$Platform_Cmd_ops['!'],
									model,
									{ctor: '[]'});
							}
						case 27:
							var _v17 = fwd,
								_v18 = _debois$elm_mdl$Material_Menu$Close,
								_v19 = model;
							fwd = _v17;
							msg = _v18;
							model = _v19;
							continue update;
						case 32:
							if (_debois$elm_mdl$Material_Menu$isActive(model)) {
								var _v20 = fwd,
									_v21 = A2(_debois$elm_mdl$Material_Menu$Key, _p27, 13),
									_v22 = model;
								fwd = _v20;
								msg = _v21;
								model = _v22;
								continue update;
							} else {
								return A2(
									_elm_lang$core$Platform_Cmd_ops['!'],
									model,
									{ctor: '[]'});
							}
						case 40:
							if (_debois$elm_mdl$Material_Menu$isActive(model)) {
								var items = A2(
									_elm_lang$core$List$indexedMap,
									F2(
										function (v0, v1) {
											return {ctor: '_Tuple2', _0: v0, _1: v1};
										}),
									_p27);
								return A3(
									_elm_lang$core$Basics$flip,
									F2(
										function (x, y) {
											return A2(_elm_lang$core$Platform_Cmd_ops['!'], x, y);
										}),
									{ctor: '[]'},
									A2(
										_elm_lang$core$Maybe$withDefault,
										model,
										A2(
											_elm_lang$core$Maybe$map,
											function (_p23) {
												return function (index_) {
													return _elm_lang$core$Native_Utils.update(
														model,
														{
															index: _elm_lang$core$Maybe$Just(index_)
														});
												}(
													_elm_lang$core$Tuple$first(_p23));
											},
											_elm_lang$core$List$head(
												A2(
													_elm_lang$core$List$filter,
													function (_p24) {
														return function (_) {
															return _.enabled;
														}(
															function (_) {
																return _.config;
															}(
																_elm_lang$core$Tuple$second(_p24)));
													},
													A2(
														_elm_lang$core$List$drop,
														1 + A2(_elm_lang$core$Maybe$withDefault, -1, model.index),
														A2(_elm_lang$core$Basics_ops['++'], items, items)))))));
							} else {
								return A2(
									_elm_lang$core$Platform_Cmd_ops['!'],
									model,
									{ctor: '[]'});
							}
						case 38:
							if (_debois$elm_mdl$Material_Menu$isActive(model)) {
								var items = A2(
									_elm_lang$core$List$indexedMap,
									F2(
										function (v0, v1) {
											return {ctor: '_Tuple2', _0: v0, _1: v1};
										}),
									_p27);
								return _debois$elm_mdl$Material_Helpers$pure(
									A2(
										_elm_lang$core$Maybe$withDefault,
										model,
										A2(
											_elm_lang$core$Maybe$map,
											function (_p25) {
												return function (index_) {
													return _elm_lang$core$Native_Utils.update(
														model,
														{
															index: _elm_lang$core$Maybe$Just(index_)
														});
												}(
													_elm_lang$core$Tuple$first(_p25));
											},
											_elm_lang$core$List$head(
												A2(
													_elm_lang$core$List$filter,
													function (_p26) {
														return function (_) {
															return _.enabled;
														}(
															function (_) {
																return _.config;
															}(
																_elm_lang$core$Tuple$second(_p26)));
													},
													A2(
														_elm_lang$core$List$drop,
														_elm_lang$core$List$length(_p27) - A2(_elm_lang$core$Maybe$withDefault, 0, model.index),
														_elm_lang$core$List$reverse(
															A2(_elm_lang$core$Basics_ops['++'], items, items))))))));
							} else {
								return A2(
									_elm_lang$core$Platform_Cmd_ops['!'],
									model,
									{ctor: '[]'});
							}
						default:
							return A2(
								_elm_lang$core$Platform_Cmd_ops['!'],
								model,
								{ctor: '[]'});
					}
			}
		}
	});
var _debois$elm_mdl$Material_Menu$react = F4(
	function (lift, msg, idx, store) {
		return A2(
			_debois$elm_mdl$Material_Helpers$map1st,
			function (_p28) {
				return _elm_lang$core$Maybe$Just(
					A3(_debois$elm_mdl$Material_Menu$set, idx, store, _p28));
			},
			A3(
				_debois$elm_mdl$Material_Menu$update,
				lift,
				msg,
				A2(_debois$elm_mdl$Material_Menu$get, idx, store)));
	});
var _debois$elm_mdl$Material_Menu$Open = function (a) {
	return {ctor: 'Open', _0: a};
};
var _debois$elm_mdl$Material_Menu$TopRight = {ctor: 'TopRight'};
var _debois$elm_mdl$Material_Menu$topRight = _debois$elm_mdl$Material_Options_Internal$option(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{alignment: _debois$elm_mdl$Material_Menu$TopRight});
	});
var _debois$elm_mdl$Material_Menu$TopLeft = {ctor: 'TopLeft'};
var _debois$elm_mdl$Material_Menu$topLeft = _debois$elm_mdl$Material_Options_Internal$option(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{alignment: _debois$elm_mdl$Material_Menu$TopLeft});
	});
var _debois$elm_mdl$Material_Menu$delay = F4(
	function (alignment, height, offsetTop, offsetHeight) {
		var t = (_elm_lang$core$Native_Utils.eq(alignment, _debois$elm_mdl$Material_Menu$TopLeft) || _elm_lang$core$Native_Utils.eq(alignment, _debois$elm_mdl$Material_Menu$TopRight)) ? ((((height - offsetTop) - offsetHeight) / height) * _debois$elm_mdl$Material_Menu$transitionDuration) : ((offsetTop / height) * _debois$elm_mdl$Material_Menu$transitionDuration);
		return A2(
			_debois$elm_mdl$Material_Options$css,
			'transition-delay',
			A2(
				_elm_lang$core$Basics_ops['++'],
				_elm_lang$core$Basics$toString(t),
				's'));
	});
var _debois$elm_mdl$Material_Menu$view1 = F8(
	function (lift, config, model, offsetTop, offsetHeight, index, summary, item) {
		var canSelect = summary.config.enabled && (!_elm_lang$core$Native_Utils.eq(summary.config.onSelect, _elm_lang$core$Maybe$Nothing));
		var hasRipple = config.ripple && canSelect;
		var ripple = function (_p29) {
			return lift(
				A2(_debois$elm_mdl$Material_Menu$Ripple, index, _p29));
		};
		return A5(
			_debois$elm_mdl$Material_Options_Internal$apply,
			summary,
			_elm_lang$html$Html$li,
			{
				ctor: '::',
				_0: _debois$elm_mdl$Material_Options$cs('mdl-menu__item'),
				_1: {
					ctor: '::',
					_0: A2(
						_debois$elm_mdl$Material_Options$when,
						config.ripple,
						_debois$elm_mdl$Material_Options$cs('mdl-js-ripple-effect')),
					_1: {
						ctor: '::',
						_0: A2(
							_debois$elm_mdl$Material_Options$when,
							summary.config.divider,
							_debois$elm_mdl$Material_Options$cs('mdl-menu__item--full-bleed-divider')),
						_1: {
							ctor: '::',
							_0: A2(
								_debois$elm_mdl$Material_Options$when,
								_elm_lang$core$Native_Utils.eq(
									model.index,
									_elm_lang$core$Maybe$Just(index)),
								A2(_debois$elm_mdl$Material_Options$css, 'background-color', 'rgb(238,238,238)')),
							_1: {
								ctor: '::',
								_0: function () {
									var _p30 = {
										ctor: '_Tuple2',
										_0: model.geometry,
										_1: _debois$elm_mdl$Material_Menu$isActive(model)
									};
									if (((_p30.ctor === '_Tuple2') && (_p30._0.ctor === 'Just')) && (_p30._1 === true)) {
										return A4(_debois$elm_mdl$Material_Menu$delay, config.alignment, _p30._0._0.menu.bounds.height, offsetTop, offsetHeight);
									} else {
										return _debois$elm_mdl$Material_Options$nop;
									}
								}(),
								_1: {
									ctor: '::',
									_0: A2(_debois$elm_mdl$Material_Options$css, 'display', 'flex'),
									_1: {
										ctor: '::',
										_0: A2(_debois$elm_mdl$Material_Options$css, 'align-items', 'center'),
										_1: {
											ctor: '::',
											_0: A2(
												_debois$elm_mdl$Material_Options$when,
												canSelect,
												_debois$elm_mdl$Material_Options$onClick(
													lift(
														A2(_debois$elm_mdl$Material_Menu$Select, index, summary.config.onSelect)))),
											_1: {
												ctor: '::',
												_0: A2(
													_debois$elm_mdl$Material_Options$when,
													!summary.config.enabled,
													_debois$elm_mdl$Material_Options_Internal$attribute(
														A2(_elm_lang$html$Html_Attributes$attribute, 'disabled', 'disabled'))),
												_1: {
													ctor: '::',
													_0: _debois$elm_mdl$Material_Options_Internal$attribute(
														A2(
															_elm_lang$html$Html_Attributes$property,
															'tabindex',
															_elm_lang$core$Json_Encode$string('-1'))),
													_1: {
														ctor: '::',
														_0: hasRipple ? _debois$elm_mdl$Material_Options$many(
															{
																ctor: '::',
																_0: _debois$elm_mdl$Material_Options_Internal$attribute(
																	A2(_debois$elm_mdl$Material_Ripple$downOn_, ripple, 'mousedown')),
																_1: {
																	ctor: '::',
																	_0: _debois$elm_mdl$Material_Options_Internal$attribute(
																		A2(_debois$elm_mdl$Material_Ripple$downOn_, ripple, 'touchstart')),
																	_1: {
																		ctor: '::',
																		_0: _debois$elm_mdl$Material_Options_Internal$attribute(
																			A2(_debois$elm_mdl$Material_Ripple$upOn_, ripple, 'mouseup')),
																		_1: {
																			ctor: '::',
																			_0: _debois$elm_mdl$Material_Options_Internal$attribute(
																				A2(_debois$elm_mdl$Material_Ripple$upOn_, ripple, 'mouseleave')),
																			_1: {
																				ctor: '::',
																				_0: _debois$elm_mdl$Material_Options_Internal$attribute(
																					A2(_debois$elm_mdl$Material_Ripple$upOn_, ripple, 'touchend')),
																				_1: {
																					ctor: '::',
																					_0: _debois$elm_mdl$Material_Options_Internal$attribute(
																						A2(_debois$elm_mdl$Material_Ripple$upOn_, ripple, 'blur')),
																					_1: {ctor: '[]'}
																				}
																			}
																		}
																	}
																}
															}) : _debois$elm_mdl$Material_Options$nop,
														_1: {ctor: '[]'}
													}
												}
											}
										}
									}
								}
							}
						}
					}
				}
			},
			{ctor: '[]'},
			hasRipple ? A2(
				F2(
					function (x, y) {
						return A2(_elm_lang$core$Basics_ops['++'], x, y);
					}),
				item.html,
				{
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$map,
						ripple,
						A2(
							_debois$elm_mdl$Material_Ripple$view_,
							{
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$class('mdl-menu__item-ripple-container'),
								_1: {ctor: '[]'}
							},
							A2(
								_elm_lang$core$Maybe$withDefault,
								_debois$elm_mdl$Material_Ripple$model,
								A2(_elm_lang$core$Dict$get, index, model.ripples)))),
					_1: {ctor: '[]'}
				}) : item.html);
	});
var _debois$elm_mdl$Material_Menu$BottomRight = {ctor: 'BottomRight'};
var _debois$elm_mdl$Material_Menu$bottomRight = _debois$elm_mdl$Material_Options_Internal$option(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{alignment: _debois$elm_mdl$Material_Menu$BottomRight});
	});
var _debois$elm_mdl$Material_Menu$BottomLeft = {ctor: 'BottomLeft'};
var _debois$elm_mdl$Material_Menu$defaultConfig = {alignment: _debois$elm_mdl$Material_Menu$BottomLeft, ripple: false, icon: 'more_vert'};
var _debois$elm_mdl$Material_Menu$bottomLeft = _debois$elm_mdl$Material_Options_Internal$option(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{alignment: _debois$elm_mdl$Material_Menu$BottomLeft});
	});
var _debois$elm_mdl$Material_Menu$containerGeometry = F2(
	function (alignment, geometry) {
		return _debois$elm_mdl$Material_Options$many(
			{
				ctor: '::',
				_0: A2(
					_debois$elm_mdl$Material_Options$css,
					'width',
					_debois$elm_mdl$Material_Menu$toPx(geometry.menu.bounds.width)),
				_1: {
					ctor: '::',
					_0: A2(
						_debois$elm_mdl$Material_Options$css,
						'height',
						_debois$elm_mdl$Material_Menu$toPx(geometry.menu.bounds.height)),
					_1: {
						ctor: '::',
						_0: (_elm_lang$core$Native_Utils.eq(alignment, _debois$elm_mdl$Material_Menu$BottomRight) || _elm_lang$core$Native_Utils.eq(alignment, _debois$elm_mdl$Material_Menu$BottomLeft)) ? A2(
							_debois$elm_mdl$Material_Options$css,
							'top',
							_debois$elm_mdl$Material_Menu$toPx(geometry.button.offsetTop + geometry.button.offsetHeight)) : _debois$elm_mdl$Material_Options$nop,
						_1: {
							ctor: '::',
							_0: function () {
								if (_elm_lang$core$Native_Utils.eq(alignment, _debois$elm_mdl$Material_Menu$BottomRight) || _elm_lang$core$Native_Utils.eq(alignment, _debois$elm_mdl$Material_Menu$TopRight)) {
									var right = function (e) {
										return e.bounds.left + e.bounds.width;
									};
									return A2(
										_debois$elm_mdl$Material_Options$css,
										'right',
										_debois$elm_mdl$Material_Menu$toPx(
											right(geometry.container) - right(geometry.menu)));
								} else {
									return _debois$elm_mdl$Material_Options$nop;
								}
							}(),
							_1: {
								ctor: '::',
								_0: function () {
									if (_elm_lang$core$Native_Utils.eq(alignment, _debois$elm_mdl$Material_Menu$TopLeft) || _elm_lang$core$Native_Utils.eq(alignment, _debois$elm_mdl$Material_Menu$TopRight)) {
										var bottom = geometry.container.bounds.top + geometry.container.bounds.height;
										return A2(
											_debois$elm_mdl$Material_Options$css,
											'bottom',
											_debois$elm_mdl$Material_Menu$toPx(bottom - geometry.button.bounds.top));
									} else {
										return _debois$elm_mdl$Material_Options$nop;
									}
								}(),
								_1: {
									ctor: '::',
									_0: (_elm_lang$core$Native_Utils.eq(alignment, _debois$elm_mdl$Material_Menu$TopLeft) || _elm_lang$core$Native_Utils.eq(alignment, _debois$elm_mdl$Material_Menu$BottomLeft)) ? A2(
										_debois$elm_mdl$Material_Options$css,
										'left',
										_debois$elm_mdl$Material_Menu$toPx(geometry.menu.offsetLeft)) : _debois$elm_mdl$Material_Options$nop,
									_1: {ctor: '[]'}
								}
							}
						}
					}
				}
			});
	});
var _debois$elm_mdl$Material_Menu$view = F4(
	function (lift, model, properties, items) {
		var itemSummaries = A2(
			_elm_lang$core$List$map,
			function (_p31) {
				return A2(
					_debois$elm_mdl$Material_Options_Internal$collect,
					_debois$elm_mdl$Material_Menu$defaultItemConfig,
					function (_) {
						return _.options;
					}(_p31));
			},
			items);
		var numItems = _elm_lang$core$List$length(items);
		var summary = A2(_debois$elm_mdl$Material_Options_Internal$collect, _debois$elm_mdl$Material_Menu$defaultConfig, properties);
		var config = summary.config;
		var alignment = function () {
			var _p32 = config.alignment;
			switch (_p32.ctor) {
				case 'BottomLeft':
					return _debois$elm_mdl$Material_Options$cs('mdl-menu--bottom-left');
				case 'BottomRight':
					return _debois$elm_mdl$Material_Options$cs('mdl-menu--bottom-right');
				case 'TopLeft':
					return _debois$elm_mdl$Material_Options$cs('mdl-menu--top-left');
				default:
					return _debois$elm_mdl$Material_Options$cs('mdl-menu--top-right');
			}
		}();
		return A5(
			_debois$elm_mdl$Material_Options_Internal$apply,
			summary,
			_elm_lang$html$Html$div,
			{
				ctor: '::',
				_0: A2(_debois$elm_mdl$Material_Options$css, 'position', 'relative'),
				_1: properties
			},
			{ctor: '[]'},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$map,
					lift,
					A3(
						_debois$elm_mdl$Material_Options$styled,
						_elm_lang$html$Html$button,
						{
							ctor: '::',
							_0: _debois$elm_mdl$Material_Options$cs('mdl-button'),
							_1: {
								ctor: '::',
								_0: _debois$elm_mdl$Material_Options$cs('mdl-js-button'),
								_1: {
									ctor: '::',
									_0: _debois$elm_mdl$Material_Options$cs('mdl-button--icon'),
									_1: {
										ctor: '::',
										_0: A2(
											_debois$elm_mdl$Material_Options$when,
											_debois$elm_mdl$Material_Menu$isActive(model),
											_debois$elm_mdl$Material_Menu$onKeyDown(
												_debois$elm_mdl$Material_Menu$Key(itemSummaries))),
										_1: {
											ctor: '::',
											_0: A2(
												_debois$elm_mdl$Material_Options$when,
												!_elm_lang$core$Native_Utils.eq(model.animationState, _debois$elm_mdl$Material_Menu$Opened),
												A2(
													_debois$elm_mdl$Material_Options$on,
													'click',
													A2(_elm_lang$core$Json_Decode$map, _debois$elm_mdl$Material_Menu$Open, _debois$elm_mdl$Material_Menu_Geometry$decode))),
											_1: {
												ctor: '::',
												_0: A2(
													_debois$elm_mdl$Material_Options$when,
													_debois$elm_mdl$Material_Menu$isActive(model),
													_debois$elm_mdl$Material_Options$onClick(_debois$elm_mdl$Material_Menu$Close)),
												_1: {ctor: '[]'}
											}
										}
									}
								}
							}
						},
						{
							ctor: '::',
							_0: A2(
								_debois$elm_mdl$Material_Icon$view,
								config.icon,
								{
									ctor: '::',
									_0: _debois$elm_mdl$Material_Options$cs('material-icons'),
									_1: {
										ctor: '::',
										_0: A2(_debois$elm_mdl$Material_Options$css, 'pointer-events', 'none'),
										_1: {ctor: '[]'}
									}
								}),
							_1: {ctor: '[]'}
						})),
				_1: {
					ctor: '::',
					_0: A3(
						_debois$elm_mdl$Material_Options$styled,
						_elm_lang$html$Html$div,
						{
							ctor: '::',
							_0: _debois$elm_mdl$Material_Options$cs('mdl-menu__container'),
							_1: {
								ctor: '::',
								_0: _debois$elm_mdl$Material_Options$cs('is-upgraded'),
								_1: {
									ctor: '::',
									_0: A2(
										_debois$elm_mdl$Material_Options$when,
										_elm_lang$core$Native_Utils.eq(model.animationState, _debois$elm_mdl$Material_Menu$Opened) || _elm_lang$core$Native_Utils.eq(model.animationState, _debois$elm_mdl$Material_Menu$Closing),
										_debois$elm_mdl$Material_Options$cs('is-visible')),
									_1: {
										ctor: '::',
										_0: A2(
											_debois$elm_mdl$Material_Menu$withGeometry,
											model,
											_debois$elm_mdl$Material_Menu$containerGeometry(config.alignment)),
										_1: {ctor: '[]'}
									}
								}
							}
						},
						{
							ctor: '::',
							_0: A3(
								_debois$elm_mdl$Material_Options$styled,
								_elm_lang$html$Html$div,
								{
									ctor: '::',
									_0: _debois$elm_mdl$Material_Options$cs('mdl-menu__outline'),
									_1: {
										ctor: '::',
										_0: alignment,
										_1: {
											ctor: '::',
											_0: A2(
												_debois$elm_mdl$Material_Menu$withGeometry,
												model,
												function (geometry) {
													return _debois$elm_mdl$Material_Options$many(
														{
															ctor: '::',
															_0: A2(
																_debois$elm_mdl$Material_Options$css,
																'width',
																_debois$elm_mdl$Material_Menu$toPx(geometry.menu.bounds.width)),
															_1: {
																ctor: '::',
																_0: A2(
																	_debois$elm_mdl$Material_Options$css,
																	'height',
																	_debois$elm_mdl$Material_Menu$toPx(geometry.menu.bounds.height)),
																_1: {ctor: '[]'}
															}
														});
												}),
											_1: {ctor: '[]'}
										}
									}
								},
								{ctor: '[]'}),
							_1: {
								ctor: '::',
								_0: A3(
									_debois$elm_mdl$Material_Options$styled,
									_elm_lang$html$Html$ul,
									{
										ctor: '::',
										_0: _debois$elm_mdl$Material_Options$cs('mdl-menu'),
										_1: {
											ctor: '::',
											_0: _debois$elm_mdl$Material_Options$cs('mdl-js-menu'),
											_1: {
												ctor: '::',
												_0: A2(
													_debois$elm_mdl$Material_Options$when,
													_elm_lang$core$Native_Utils.eq(model.animationState, _debois$elm_mdl$Material_Menu$Opening) || _elm_lang$core$Native_Utils.eq(model.animationState, _debois$elm_mdl$Material_Menu$Closing),
													_debois$elm_mdl$Material_Options$cs('is-animating')),
												_1: {
													ctor: '::',
													_0: A2(
														_debois$elm_mdl$Material_Menu$withGeometry,
														model,
														A2(_debois$elm_mdl$Material_Menu$clip, model, config)),
													_1: {
														ctor: '::',
														_0: alignment,
														_1: {ctor: '[]'}
													}
												}
											}
										}
									},
									function () {
										var _p33 = model.geometry;
										if (_p33.ctor === 'Just') {
											var _p34 = _p33._0;
											return A6(
												_elm_lang$core$List$map5,
												A3(_debois$elm_mdl$Material_Menu$view1, lift, config, model),
												_p34.offsetTops,
												_p34.offsetHeights,
												A2(_elm_lang$core$List$range, 0, numItems - 1),
												itemSummaries,
												items);
										} else {
											return A4(
												_elm_lang$core$List$map3,
												A5(_debois$elm_mdl$Material_Menu$view1, lift, config, model, 0, 0),
												A2(_elm_lang$core$List$range, 0, numItems - 1),
												itemSummaries,
												items);
										}
									}()),
								_1: {ctor: '[]'}
							}
						}),
					_1: {ctor: '[]'}
				}
			});
	});
var _debois$elm_mdl$Material_Menu$render = A3(_debois$elm_mdl$Material_Component$render, _debois$elm_mdl$Material_Menu$get, _debois$elm_mdl$Material_Menu$view, _debois$elm_mdl$Material_Component$MenuMsg);

var _debois$elm_mdl$Material_Snackbar$enqueue = F2(
	function (contents, model) {
		return _elm_lang$core$Native_Utils.update(
			model,
			{
				queue: A2(
					_elm_lang$core$List$append,
					model.queue,
					{
						ctor: '::',
						_0: contents,
						_1: {ctor: '[]'}
					})
			});
	});
var _debois$elm_mdl$Material_Snackbar$snackbar = F3(
	function (payload, message, label) {
		return {
			message: message,
			action: _elm_lang$core$Maybe$Just(label),
			payload: payload,
			timeout: 2750,
			fade: 250
		};
	});
var _debois$elm_mdl$Material_Snackbar$toast = F2(
	function (payload, message) {
		return {message: message, action: _elm_lang$core$Maybe$Nothing, payload: payload, timeout: 2750, fade: 250};
	});
var _debois$elm_mdl$Material_Snackbar$Contents = F5(
	function (a, b, c, d, e) {
		return {message: a, action: b, payload: c, timeout: d, fade: e};
	});
var _debois$elm_mdl$Material_Snackbar$Model = F3(
	function (a, b, c) {
		return {queue: a, state: b, seq: c};
	});
var _debois$elm_mdl$Material_Snackbar$Fading = function (a) {
	return {ctor: 'Fading', _0: a};
};
var _debois$elm_mdl$Material_Snackbar$Active = function (a) {
	return {ctor: 'Active', _0: a};
};
var _debois$elm_mdl$Material_Snackbar$Inert = {ctor: 'Inert'};
var _debois$elm_mdl$Material_Snackbar$model = {
	queue: {ctor: '[]'},
	state: _debois$elm_mdl$Material_Snackbar$Inert,
	seq: -1
};
var _debois$elm_mdl$Material_Snackbar$Clicked = {ctor: 'Clicked'};
var _debois$elm_mdl$Material_Snackbar$Timeout = {ctor: 'Timeout'};
var _debois$elm_mdl$Material_Snackbar$Move = F2(
	function (a, b) {
		return {ctor: 'Move', _0: a, _1: b};
	});
var _debois$elm_mdl$Material_Snackbar$next = function (model) {
	return _elm_lang$core$Platform_Cmd$map(
		_debois$elm_mdl$Material_Snackbar$Move(model.seq));
};
var _debois$elm_mdl$Material_Snackbar$view = function (model) {
	var isActive = function () {
		var _p0 = model.state;
		switch (_p0.ctor) {
			case 'Inert':
				return false;
			case 'Active':
				return true;
			default:
				return false;
		}
	}();
	var contents = function () {
		var _p1 = model.state;
		switch (_p1.ctor) {
			case 'Inert':
				return _elm_lang$core$Maybe$Nothing;
			case 'Active':
				return _elm_lang$core$Maybe$Just(_p1._0);
			default:
				return _elm_lang$core$Maybe$Just(_p1._0);
		}
	}();
	var action = A2(
		_elm_lang$core$Maybe$andThen,
		function (_) {
			return _.action;
		},
		contents);
	return A2(
		_elm_lang$html$Html$div,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$classList(
				{
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: 'mdl-js-snackbar', _1: true},
					_1: {
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: 'mdl-snackbar', _1: true},
						_1: {
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'mdl-snackbar--active', _1: isActive},
							_1: {ctor: '[]'}
						}
					}
				}),
			_1: {
				ctor: '::',
				_0: A2(_debois$elm_mdl$Material_Helpers$aria, 'hidden', !isActive),
				_1: {ctor: '[]'}
			}
		},
		{
			ctor: '::',
			_0: A2(
				_elm_lang$html$Html$div,
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$class('mdl-snackbar__text'),
					_1: {ctor: '[]'}
				},
				A2(
					_elm_lang$core$Maybe$withDefault,
					{ctor: '[]'},
					A2(
						_elm_lang$core$Maybe$map,
						function (c) {
							return {
								ctor: '::',
								_0: _elm_lang$html$Html$text(c.message),
								_1: {ctor: '[]'}
							};
						},
						contents))),
			_1: {
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$button,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$class('mdl-snackbar__action'),
						_1: {
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$type_('button'),
							_1: {
								ctor: '::',
								_0: A2(
									_debois$elm_mdl$Material_Helpers$aria,
									'hidden',
									A2(
										_elm_lang$core$Maybe$withDefault,
										true,
										A2(
											_elm_lang$core$Maybe$map,
											_elm_lang$core$Basics$always(!isActive),
											action))),
								_1: A2(
									_elm_lang$core$Maybe$withDefault,
									{ctor: '[]'},
									A2(
										_elm_lang$core$Maybe$map,
										_elm_lang$core$Basics$always(
											{
												ctor: '::',
												_0: _elm_lang$html$Html_Events$onClick(
													A2(_debois$elm_mdl$Material_Snackbar$Move, model.seq, _debois$elm_mdl$Material_Snackbar$Clicked)),
												_1: {ctor: '[]'}
											}),
										action))
							}
						}
					},
					A2(
						_elm_lang$core$Maybe$withDefault,
						{ctor: '[]'},
						A2(
							_elm_lang$core$Maybe$map,
							function (action) {
								return {
									ctor: '::',
									_0: _elm_lang$html$Html$text(action),
									_1: {ctor: '[]'}
								};
							},
							action))),
				_1: {ctor: '[]'}
			}
		});
};
var _debois$elm_mdl$Material_Snackbar$Click = function (a) {
	return {ctor: 'Click', _0: a};
};
var _debois$elm_mdl$Material_Snackbar$End = function (a) {
	return {ctor: 'End', _0: a};
};
var _debois$elm_mdl$Material_Snackbar$Begin = function (a) {
	return {ctor: 'Begin', _0: a};
};
var _debois$elm_mdl$Material_Snackbar$tryDequeue = function (model) {
	var _p2 = {ctor: '_Tuple2', _0: model.state, _1: model.queue};
	if (((_p2.ctor === '_Tuple2') && (_p2._0.ctor === 'Inert')) && (_p2._1.ctor === '::')) {
		var _p3 = _p2._1._0;
		return {
			ctor: '_Tuple2',
			_0: _elm_lang$core$Native_Utils.update(
				model,
				{
					state: _debois$elm_mdl$Material_Snackbar$Active(_p3),
					queue: _p2._1._1,
					seq: model.seq + 1
				}),
			_1: _elm_lang$core$Platform_Cmd$batch(
				{
					ctor: '::',
					_0: A2(
						_elm_lang$core$Platform_Cmd$map,
						_debois$elm_mdl$Material_Snackbar$Move(model.seq + 1),
						A2(_debois$elm_mdl$Material_Helpers$delay, _p3.timeout, _debois$elm_mdl$Material_Snackbar$Timeout)),
					_1: {
						ctor: '::',
						_0: _debois$elm_mdl$Material_Helpers$cmd(
							_debois$elm_mdl$Material_Snackbar$Begin(_p3.payload)),
						_1: {ctor: '[]'}
					}
				})
		};
	} else {
		return {ctor: '_Tuple2', _0: model, _1: _elm_lang$core$Platform_Cmd$none};
	}
};
var _debois$elm_mdl$Material_Snackbar$move = F2(
	function (transition, model) {
		var _p4 = {ctor: '_Tuple2', _0: model.state, _1: transition};
		_v3_4:
		do {
			if (_p4.ctor === '_Tuple2') {
				if (_p4._1.ctor === 'Clicked') {
					if (_p4._0.ctor === 'Active') {
						var _p5 = _p4._0._0;
						return {
							ctor: '_Tuple2',
							_0: _elm_lang$core$Native_Utils.update(
								model,
								{
									state: _debois$elm_mdl$Material_Snackbar$Fading(_p5)
								}),
							_1: _elm_lang$core$Platform_Cmd$batch(
								{
									ctor: '::',
									_0: A2(
										_debois$elm_mdl$Material_Snackbar$next,
										model,
										A2(_debois$elm_mdl$Material_Helpers$delay, _p5.fade, _debois$elm_mdl$Material_Snackbar$Timeout)),
									_1: {
										ctor: '::',
										_0: _debois$elm_mdl$Material_Helpers$cmd(
											_debois$elm_mdl$Material_Snackbar$Click(_p5.payload)),
										_1: {ctor: '[]'}
									}
								})
						};
					} else {
						break _v3_4;
					}
				} else {
					switch (_p4._0.ctor) {
						case 'Inert':
							return _debois$elm_mdl$Material_Snackbar$tryDequeue(model);
						case 'Active':
							var _p6 = _p4._0._0;
							return {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Native_Utils.update(
									model,
									{
										state: _debois$elm_mdl$Material_Snackbar$Fading(_p6)
									}),
								_1: _elm_lang$core$Platform_Cmd$batch(
									{
										ctor: '::',
										_0: A2(
											_debois$elm_mdl$Material_Snackbar$next,
											model,
											A2(_debois$elm_mdl$Material_Helpers$delay, _p6.fade, _debois$elm_mdl$Material_Snackbar$Timeout)),
										_1: {ctor: '[]'}
									})
							};
						default:
							return {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Native_Utils.update(
									model,
									{state: _debois$elm_mdl$Material_Snackbar$Inert}),
								_1: _elm_lang$core$Platform_Cmd$batch(
									{
										ctor: '::',
										_0: A2(
											_debois$elm_mdl$Material_Snackbar$next,
											model,
											_debois$elm_mdl$Material_Helpers$cmd(_debois$elm_mdl$Material_Snackbar$Timeout)),
										_1: {
											ctor: '::',
											_0: _debois$elm_mdl$Material_Helpers$cmd(
												_debois$elm_mdl$Material_Snackbar$End(_p4._0._0.payload)),
											_1: {ctor: '[]'}
										}
									})
							};
					}
				}
			} else {
				break _v3_4;
			}
		} while(false);
		return {ctor: '_Tuple2', _0: model, _1: _elm_lang$core$Platform_Cmd$none};
	});
var _debois$elm_mdl$Material_Snackbar$update = F2(
	function (action, model) {
		var _p7 = action;
		if (_p7.ctor === 'Move') {
			return _elm_lang$core$Native_Utils.eq(_p7._0, model.seq) ? A2(_debois$elm_mdl$Material_Snackbar$move, _p7._1, model) : {ctor: '_Tuple2', _0: model, _1: _elm_lang$core$Platform_Cmd$none};
		} else {
			return {ctor: '_Tuple2', _0: model, _1: _elm_lang$core$Platform_Cmd$none};
		}
	});
var _debois$elm_mdl$Material_Snackbar$add = F2(
	function (contents, model) {
		return _debois$elm_mdl$Material_Snackbar$tryDequeue(
			A2(_debois$elm_mdl$Material_Snackbar$enqueue, contents, model));
	});

var _elm_lang$html$Html_Keyed$node = _elm_lang$virtual_dom$VirtualDom$keyedNode;
var _elm_lang$html$Html_Keyed$ol = _elm_lang$html$Html_Keyed$node('ol');
var _elm_lang$html$Html_Keyed$ul = _elm_lang$html$Html_Keyed$node('ul');

var _elm_lang$window$Native_Window = function()
{

var size = _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)	{
	callback(_elm_lang$core$Native_Scheduler.succeed({
		width: window.innerWidth,
		height: window.innerHeight
	}));
});

return {
	size: size
};

}();
var _elm_lang$window$Window_ops = _elm_lang$window$Window_ops || {};
_elm_lang$window$Window_ops['&>'] = F2(
	function (task1, task2) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (_p0) {
				return task2;
			},
			task1);
	});
var _elm_lang$window$Window$onSelfMsg = F3(
	function (router, dimensions, state) {
		var _p1 = state;
		if (_p1.ctor === 'Nothing') {
			return _elm_lang$core$Task$succeed(state);
		} else {
			var send = function (_p2) {
				var _p3 = _p2;
				return A2(
					_elm_lang$core$Platform$sendToApp,
					router,
					_p3._0(dimensions));
			};
			return A2(
				_elm_lang$window$Window_ops['&>'],
				_elm_lang$core$Task$sequence(
					A2(_elm_lang$core$List$map, send, _p1._0.subs)),
				_elm_lang$core$Task$succeed(state));
		}
	});
var _elm_lang$window$Window$init = _elm_lang$core$Task$succeed(_elm_lang$core$Maybe$Nothing);
var _elm_lang$window$Window$size = _elm_lang$window$Native_Window.size;
var _elm_lang$window$Window$width = A2(
	_elm_lang$core$Task$map,
	function (_) {
		return _.width;
	},
	_elm_lang$window$Window$size);
var _elm_lang$window$Window$height = A2(
	_elm_lang$core$Task$map,
	function (_) {
		return _.height;
	},
	_elm_lang$window$Window$size);
var _elm_lang$window$Window$onEffects = F3(
	function (router, newSubs, oldState) {
		var _p4 = {ctor: '_Tuple2', _0: oldState, _1: newSubs};
		if (_p4._0.ctor === 'Nothing') {
			if (_p4._1.ctor === '[]') {
				return _elm_lang$core$Task$succeed(_elm_lang$core$Maybe$Nothing);
			} else {
				return A2(
					_elm_lang$core$Task$andThen,
					function (pid) {
						return _elm_lang$core$Task$succeed(
							_elm_lang$core$Maybe$Just(
								{subs: newSubs, pid: pid}));
					},
					_elm_lang$core$Process$spawn(
						A3(
							_elm_lang$dom$Dom_LowLevel$onWindow,
							'resize',
							_elm_lang$core$Json_Decode$succeed(
								{ctor: '_Tuple0'}),
							function (_p5) {
								return A2(
									_elm_lang$core$Task$andThen,
									_elm_lang$core$Platform$sendToSelf(router),
									_elm_lang$window$Window$size);
							})));
			}
		} else {
			if (_p4._1.ctor === '[]') {
				return A2(
					_elm_lang$window$Window_ops['&>'],
					_elm_lang$core$Process$kill(_p4._0._0.pid),
					_elm_lang$core$Task$succeed(_elm_lang$core$Maybe$Nothing));
			} else {
				return _elm_lang$core$Task$succeed(
					_elm_lang$core$Maybe$Just(
						{subs: newSubs, pid: _p4._0._0.pid}));
			}
		}
	});
var _elm_lang$window$Window$subscription = _elm_lang$core$Native_Platform.leaf('Window');
var _elm_lang$window$Window$Size = F2(
	function (a, b) {
		return {width: a, height: b};
	});
var _elm_lang$window$Window$MySub = function (a) {
	return {ctor: 'MySub', _0: a};
};
var _elm_lang$window$Window$resizes = function (tagger) {
	return _elm_lang$window$Window$subscription(
		_elm_lang$window$Window$MySub(tagger));
};
var _elm_lang$window$Window$subMap = F2(
	function (func, _p6) {
		var _p7 = _p6;
		return _elm_lang$window$Window$MySub(
			function (_p8) {
				return func(
					_p7._0(_p8));
			});
	});
_elm_lang$core$Native_Platform.effectManagers['Window'] = {pkg: 'elm-lang/window', init: _elm_lang$window$Window$init, onEffects: _elm_lang$window$Window$onEffects, onSelfMsg: _elm_lang$window$Window$onSelfMsg, tag: 'sub', subMap: _elm_lang$window$Window$subMap};

var _debois$elm_mdl$Material_Layout$_p0 = {
	ctor: '_Tuple2',
	_0: function (_) {
		return _.layout;
	},
	_1: F2(
		function (x, s) {
			return _elm_lang$core$Native_Utils.update(
				s,
				{layout: x});
		})
};
var _debois$elm_mdl$Material_Layout$get = _debois$elm_mdl$Material_Layout$_p0._0;
var _debois$elm_mdl$Material_Layout$set = _debois$elm_mdl$Material_Layout$_p0._1;
var _debois$elm_mdl$Material_Layout$drawerView = F3(
	function (lift, isVisible, elems) {
		return A2(
			_elm_lang$html$Html$div,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$classList(
					{
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: 'mdl-layout__drawer', _1: true},
						_1: {
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'is-visible', _1: isVisible},
							_1: {ctor: '[]'}
						}
					}),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html_Attributes$attribute,
						'aria-hidden',
						isVisible ? 'false' : 'true'),
					_1: {ctor: '[]'}
				}
			},
			elems);
	});
var _debois$elm_mdl$Material_Layout$onKeypressFilterSpaceAndEnter = A2(_elm_lang$html$Html_Attributes$attribute, 'onkeypress', '\n  (function (evt) {\n     if (evt && evt.type === \"keydown\" && (evt.keyCode === 32 || evt.keyCode === 13)) {\n       evt.preventDefault();\n     }\n   })(window.event);\n  ');
var _debois$elm_mdl$Material_Layout$toList = function (x) {
	var _p1 = x;
	if (_p1.ctor === 'Nothing') {
		return {ctor: '[]'};
	} else {
		return {
			ctor: '::',
			_0: _p1._0,
			_1: {ctor: '[]'}
		};
	}
};
var _debois$elm_mdl$Material_Layout$isWaterfall = function (mode) {
	var _p2 = mode;
	if (_p2.ctor === 'Waterfall') {
		return true;
	} else {
		return false;
	}
};
var _debois$elm_mdl$Material_Layout$row = function (styles) {
	return _debois$elm_mdl$Material_Options$div(
		{
			ctor: '::',
			_0: _debois$elm_mdl$Material_Options$cs('mdl-layout__header-row'),
			_1: styles
		});
};
var _debois$elm_mdl$Material_Layout$link = F2(
	function (styles, contents) {
		return A3(
			_debois$elm_mdl$Material_Options$styled,
			_elm_lang$html$Html$a,
			{
				ctor: '::',
				_0: _debois$elm_mdl$Material_Options$cs('mdl-navigation__link'),
				_1: {
					ctor: '::',
					_0: _debois$elm_mdl$Material_Options_Internal$attribute(
						A2(_elm_lang$html$Html_Attributes$attribute, 'tabindex', '1')),
					_1: styles
				}
			},
			contents);
	});
var _debois$elm_mdl$Material_Layout$href = function (url) {
	return _debois$elm_mdl$Material_Options$attribute(
		_elm_lang$html$Html_Attributes$href(url));
};
var _debois$elm_mdl$Material_Layout$navigation = F2(
	function (styles, contents) {
		return A3(
			_debois$elm_mdl$Material_Options$styled,
			_elm_lang$html$Html$nav,
			{
				ctor: '::',
				_0: _debois$elm_mdl$Material_Options$cs('mdl-navigation'),
				_1: styles
			},
			contents);
	});
var _debois$elm_mdl$Material_Layout$title = function (styles) {
	return _debois$elm_mdl$Material_Options$span(
		{
			ctor: '::',
			_0: _debois$elm_mdl$Material_Options$cs('mdl-layout__title'),
			_1: styles
		});
};
var _debois$elm_mdl$Material_Layout$spacer = A2(
	_elm_lang$html$Html$div,
	{
		ctor: '::',
		_0: _elm_lang$html$Html_Attributes$class('mdl-layout-spacer'),
		_1: {ctor: '[]'}
	},
	{ctor: '[]'});
var _debois$elm_mdl$Material_Layout$onSelectTab = function (_p3) {
	return _debois$elm_mdl$Material_Options_Internal$option(
		F2(
			function (f, config) {
				return _elm_lang$core$Native_Utils.update(
					config,
					{
						onSelectTab: _elm_lang$core$Maybe$Just(
							function (_p4) {
								return _elm_lang$html$Html_Events$onClick(
									f(_p4));
							})
					});
			})(_p3));
};
var _debois$elm_mdl$Material_Layout$moreTabs = _debois$elm_mdl$Material_Options_Internal$option(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{moreTabs: true});
	});
var _debois$elm_mdl$Material_Layout$selectedTab = function (_p5) {
	return _debois$elm_mdl$Material_Options_Internal$option(
		F2(
			function (k, config) {
				return _elm_lang$core$Native_Utils.update(
					config,
					{selectedTab: k});
			})(_p5));
};
var _debois$elm_mdl$Material_Layout$transparentHeader = _debois$elm_mdl$Material_Options_Internal$option(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{transparentHeader: true});
	});
var _debois$elm_mdl$Material_Layout$rippleTabs = _debois$elm_mdl$Material_Options_Internal$option(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{rippleTabs: true});
	});
var _debois$elm_mdl$Material_Layout$fixedTabs = _debois$elm_mdl$Material_Options_Internal$option(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{fixedTabs: true});
	});
var _debois$elm_mdl$Material_Layout$fixedDrawer = _debois$elm_mdl$Material_Options_Internal$option(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{fixedDrawer: true});
	});
var _debois$elm_mdl$Material_Layout$fixedHeader = _debois$elm_mdl$Material_Options_Internal$option(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{fixedHeader: true});
	});
var _debois$elm_mdl$Material_Layout$mainId = 'elm-mdl-layout-main';
var _debois$elm_mdl$Material_Layout$setTabsWidth_ = F2(
	function (width, model) {
		var x = model.tabScrollState;
		return _elm_lang$core$Native_Utils.update(
			model,
			{
				tabScrollState: _elm_lang$core$Native_Utils.update(
					x,
					{
						width: _elm_lang$core$Maybe$Just(width)
					})
			});
	});
var _debois$elm_mdl$Material_Layout$setTabsWidth = F2(
	function (w, container) {
		return _elm_lang$core$Native_Utils.update(
			container,
			{
				layout: A2(_debois$elm_mdl$Material_Layout$setTabsWidth_, w, container.layout)
			});
	});
var _debois$elm_mdl$Material_Layout$defaultTabScrollState = {canScrollRight: true, canScrollLeft: false, width: _elm_lang$core$Maybe$Nothing};
var _debois$elm_mdl$Material_Layout$defaultModel = {ripples: _elm_lang$core$Dict$empty, isSmallScreen: false, isCompact: false, isAnimating: false, isScrolled: false, isDrawerOpen: false, tabScrollState: _debois$elm_mdl$Material_Layout$defaultTabScrollState};
var _debois$elm_mdl$Material_Layout$TabScrollState = F3(
	function (a, b, c) {
		return {canScrollLeft: a, canScrollRight: b, width: c};
	});
var _debois$elm_mdl$Material_Layout$Model = F7(
	function (a, b, c, d, e, f, g) {
		return {ripples: a, isSmallScreen: b, isCompact: c, isAnimating: d, isScrolled: e, isDrawerOpen: f, tabScrollState: g};
	});
var _debois$elm_mdl$Material_Layout$Config = F9(
	function (a, b, c, d, e, f, g, h, i) {
		return {fixedHeader: a, fixedDrawer: b, fixedTabs: c, rippleTabs: d, mode: e, selectedTab: f, onSelectTab: g, transparentHeader: h, moreTabs: i};
	});
var _debois$elm_mdl$Material_Layout$Contents = F4(
	function (a, b, c, d) {
		return {header: a, drawer: b, tabs: c, main: d};
	});
var _debois$elm_mdl$Material_Layout$Ripple = F2(
	function (a, b) {
		return {ctor: 'Ripple', _0: a, _1: b};
	});
var _debois$elm_mdl$Material_Layout$NOP = {ctor: 'NOP'};
var _debois$elm_mdl$Material_Layout$TransitionEnd = {ctor: 'TransitionEnd'};
var _debois$elm_mdl$Material_Layout$TransitionHeader = function (a) {
	return {ctor: 'TransitionHeader', _0: a};
};
var _debois$elm_mdl$Material_Layout$update_ = F3(
	function (f, action, model) {
		update_:
		while (true) {
			var _p6 = action;
			switch (_p6.ctor) {
				case 'NOP':
					return _elm_lang$core$Maybe$Nothing;
				case 'Resize':
					var _p7 = _p6._0;
					var tabScrollState = A2(
						_elm_lang$core$Maybe$withDefault,
						model.tabScrollState,
						A2(
							_elm_lang$core$Maybe$map,
							function (tabsWidth) {
								var tabScrollState = model.tabScrollState;
								return _elm_lang$core$Native_Utils.update(
									tabScrollState,
									{
										canScrollRight: _elm_lang$core$Native_Utils.cmp(tabsWidth + (2 * 56), _p7) > 0
									});
							},
							model.tabScrollState.width));
					var isSmall = _elm_lang$core$Native_Utils.cmp(1024, _p7) > -1;
					return (_elm_lang$core$Native_Utils.eq(isSmall, model.isSmallScreen) && _elm_lang$core$Native_Utils.eq(tabScrollState.canScrollRight, model.tabScrollState.canScrollRight)) ? _elm_lang$core$Maybe$Nothing : _elm_lang$core$Maybe$Just(
						_debois$elm_mdl$Material_Helpers$pure(
							_elm_lang$core$Native_Utils.update(
								model,
								{isSmallScreen: isSmall, isDrawerOpen: (!isSmall) && model.isDrawerOpen, tabScrollState: tabScrollState})));
				case 'ToggleDrawer':
					return _elm_lang$core$Maybe$Just(
						_debois$elm_mdl$Material_Helpers$pure(
							_elm_lang$core$Native_Utils.update(
								model,
								{isDrawerOpen: !model.isDrawerOpen})));
				case 'Ripple':
					var _p9 = _p6._0;
					return _elm_lang$core$Maybe$Just(
						A2(
							_debois$elm_mdl$Material_Helpers$map2nd,
							_elm_lang$core$Platform_Cmd$map(
								function (_p8) {
									return f(
										A2(_debois$elm_mdl$Material_Layout$Ripple, _p9, _p8));
								}),
							A2(
								_debois$elm_mdl$Material_Helpers$map1st,
								function (ripple_) {
									return _elm_lang$core$Native_Utils.update(
										model,
										{
											ripples: A3(_elm_lang$core$Dict$insert, _p9, ripple_, model.ripples)
										});
								},
								A2(
									_debois$elm_mdl$Material_Ripple$update,
									_p6._1,
									A2(
										_elm_lang$core$Maybe$withDefault,
										_debois$elm_mdl$Material_Ripple$model,
										A2(_elm_lang$core$Dict$get, _p9, model.ripples))))));
				case 'ScrollTab':
					var _p10 = _p6._0;
					return (!_elm_lang$core$Native_Utils.eq(model.tabScrollState, _p10)) ? _elm_lang$core$Maybe$Just(
						_debois$elm_mdl$Material_Helpers$pure(
							_elm_lang$core$Native_Utils.update(
								model,
								{tabScrollState: _p10}))) : _elm_lang$core$Maybe$Nothing;
				case 'ScrollPane':
					var isScrolled = _elm_lang$core$Native_Utils.cmp(0.0, _p6._1) < 0;
					if (!_elm_lang$core$Native_Utils.eq(isScrolled, model.isScrolled)) {
						var _v3 = f,
							_v4 = _debois$elm_mdl$Material_Layout$TransitionHeader(
							{toCompact: isScrolled, fixedHeader: _p6._0}),
							_v5 = _elm_lang$core$Native_Utils.update(
							model,
							{isScrolled: isScrolled});
						f = _v3;
						action = _v4;
						model = _v5;
						continue update_;
					} else {
						return _elm_lang$core$Maybe$Nothing;
					}
				case 'TransitionHeader':
					return (!model.isAnimating) ? _elm_lang$core$Maybe$Just(
						{
							ctor: '_Tuple2',
							_0: _elm_lang$core$Native_Utils.update(
								model,
								{isCompact: _p6._0.toCompact, isAnimating: (!model.isSmallScreen) || _p6._0.fixedHeader}),
							_1: _elm_lang$core$Platform_Cmd$none
						}) : _elm_lang$core$Maybe$Nothing;
				default:
					return _elm_lang$core$Maybe$Just(
						_debois$elm_mdl$Material_Helpers$pure(
							_elm_lang$core$Native_Utils.update(
								model,
								{isAnimating: false})));
			}
		}
	});
var _debois$elm_mdl$Material_Layout$update = F2(
	function (msg, model) {
		return A2(
			_elm_lang$core$Maybe$withDefault,
			{ctor: '_Tuple2', _0: model, _1: _elm_lang$core$Platform_Cmd$none},
			A3(_debois$elm_mdl$Material_Layout$update_, _elm_lang$core$Basics$identity, msg, model));
	});
var _debois$elm_mdl$Material_Layout$react = F3(
	function (lift, msg, store) {
		var _p11 = A3(
			_debois$elm_mdl$Material_Layout$update_,
			lift,
			msg,
			_debois$elm_mdl$Material_Layout$get(store));
		if ((_p11.ctor === 'Just') && (_p11._0.ctor === '_Tuple2')) {
			return {
				ctor: '_Tuple2',
				_0: _elm_lang$core$Maybe$Just(
					A2(_debois$elm_mdl$Material_Layout$set, _p11._0._0, store)),
				_1: _p11._0._1
			};
		} else {
			return {ctor: '_Tuple2', _0: _elm_lang$core$Maybe$Nothing, _1: _elm_lang$core$Platform_Cmd$none};
		}
	});
var _debois$elm_mdl$Material_Layout$ScrollPane = F2(
	function (a, b) {
		return {ctor: 'ScrollPane', _0: a, _1: b};
	});
var _debois$elm_mdl$Material_Layout$ScrollTab = function (a) {
	return {ctor: 'ScrollTab', _0: a};
};
var _debois$elm_mdl$Material_Layout$Resize = function (a) {
	return {ctor: 'Resize', _0: a};
};
var _debois$elm_mdl$Material_Layout$init = function () {
	var measureScreenSize = A2(_elm_lang$core$Task$perform, _debois$elm_mdl$Material_Layout$Resize, _elm_lang$window$Window$width);
	return {ctor: '_Tuple2', _0: _debois$elm_mdl$Material_Layout$defaultModel, _1: measureScreenSize};
}();
var _debois$elm_mdl$Material_Layout$sub0 = function (lift) {
	return A2(
		_elm_lang$core$Platform_Cmd$map,
		function (_p12) {
			return lift(
				_debois$elm_mdl$Material_Component$LayoutMsg(_p12));
		},
		_elm_lang$core$Tuple$second(_debois$elm_mdl$Material_Layout$init));
};
var _debois$elm_mdl$Material_Layout$subscriptions = function (model) {
	return _elm_lang$window$Window$resizes(
		function (_p13) {
			return _debois$elm_mdl$Material_Layout$Resize(
				function (_) {
					return _.width;
				}(_p13));
		});
};
var _debois$elm_mdl$Material_Layout$subs = function (lift) {
	return function (_p14) {
		return A2(
			_elm_lang$core$Platform_Sub$map,
			function (_p15) {
				return lift(
					_debois$elm_mdl$Material_Component$LayoutMsg(_p15));
			},
			_debois$elm_mdl$Material_Layout$subscriptions(
				_debois$elm_mdl$Material_Layout$get(_p14)));
	};
};
var _debois$elm_mdl$Material_Layout$ToggleDrawer = {ctor: 'ToggleDrawer'};
var _debois$elm_mdl$Material_Layout$drawerButton = F2(
	function (lift, isVisible) {
		return A2(
			_elm_lang$html$Html$div,
			{ctor: '[]'},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$div,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$classList(
							{
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: 'mdl-layout__drawer-button', _1: true},
								_1: {ctor: '[]'}
							}),
						_1: {
							ctor: '::',
							_0: A2(
								_elm_lang$html$Html_Attributes$attribute,
								'aria-expanded',
								isVisible ? 'true' : 'false'),
							_1: {
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$tabindex(1),
								_1: {
									ctor: '::',
									_0: _elm_lang$html$Html_Events$onClick(
										lift(_debois$elm_mdl$Material_Layout$ToggleDrawer)),
									_1: {
										ctor: '::',
										_0: A3(
											_elm_lang$html$Html_Events$onWithOptions,
											'keydown',
											{stopPropagation: false, preventDefault: false},
											A2(
												_elm_lang$core$Json_Decode$map,
												function (_p16) {
													return lift(
														function (key) {
															var _p17 = key;
															switch (_p17) {
																case 32:
																	return _debois$elm_mdl$Material_Layout$ToggleDrawer;
																case 13:
																	return _debois$elm_mdl$Material_Layout$ToggleDrawer;
																default:
																	return _debois$elm_mdl$Material_Layout$NOP;
															}
														}(_p16));
												},
												_elm_lang$html$Html_Events$keyCode)),
										_1: {ctor: '[]'}
									}
								}
							}
						}
					},
					{
						ctor: '::',
						_0: _debois$elm_mdl$Material_Icon$i('menu'),
						_1: {ctor: '[]'}
					}),
				_1: {ctor: '[]'}
			});
	});
var _debois$elm_mdl$Material_Layout$obfuscator = F2(
	function (lift, isVisible) {
		return A2(
			_elm_lang$html$Html$div,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$classList(
					{
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: 'mdl-layout__obfuscator', _1: true},
						_1: {
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'is-visible', _1: isVisible},
							_1: {ctor: '[]'}
						}
					}),
				_1: {
					ctor: '::',
					_0: _elm_lang$html$Html_Events$onClick(
						lift(_debois$elm_mdl$Material_Layout$ToggleDrawer)),
					_1: {ctor: '[]'}
				}
			},
			{ctor: '[]'});
	});
var _debois$elm_mdl$Material_Layout$toggleDrawer = function (lift) {
	return function (_p18) {
		return lift(
			_debois$elm_mdl$Material_Component$LayoutMsg(_p18));
	}(_debois$elm_mdl$Material_Layout$ToggleDrawer);
};
var _debois$elm_mdl$Material_Layout$Waterfall = function (a) {
	return {ctor: 'Waterfall', _0: a};
};
var _debois$elm_mdl$Material_Layout$waterfall = function (_p19) {
	return _debois$elm_mdl$Material_Options_Internal$option(
		F2(
			function (b, config) {
				return _elm_lang$core$Native_Utils.update(
					config,
					{
						mode: _debois$elm_mdl$Material_Layout$Waterfall(b)
					});
			})(_p19));
};
var _debois$elm_mdl$Material_Layout$Scrolling = {ctor: 'Scrolling'};
var _debois$elm_mdl$Material_Layout$scrolling = _debois$elm_mdl$Material_Options_Internal$option(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{mode: _debois$elm_mdl$Material_Layout$Scrolling});
	});
var _debois$elm_mdl$Material_Layout$Seamed = {ctor: 'Seamed'};
var _debois$elm_mdl$Material_Layout$seamed = _debois$elm_mdl$Material_Options_Internal$option(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{mode: _debois$elm_mdl$Material_Layout$Seamed});
	});
var _debois$elm_mdl$Material_Layout$Standard = {ctor: 'Standard'};
var _debois$elm_mdl$Material_Layout$defaultConfig = {fixedHeader: false, fixedDrawer: false, fixedTabs: false, rippleTabs: true, mode: _debois$elm_mdl$Material_Layout$Standard, onSelectTab: _elm_lang$core$Maybe$Nothing, selectedTab: -1, moreTabs: false, transparentHeader: false};
var _debois$elm_mdl$Material_Layout$headerView = F4(
	function (lift, config, model, _p20) {
		var _p21 = _p20;
		var mode = function () {
			var _p22 = config.mode;
			switch (_p22.ctor) {
				case 'Standard':
					return _debois$elm_mdl$Material_Options$nop;
				case 'Scrolling':
					return _debois$elm_mdl$Material_Options$cs('mdl-layout__header--scroll');
				case 'Seamed':
					return _debois$elm_mdl$Material_Options$cs('mdl-layout__header--seamed');
				default:
					if (_p22._0 === true) {
						return _debois$elm_mdl$Material_Options$cs('mdl-layout__header--waterfall mdl-layout__header--waterfall-hide-top');
					} else {
						return _debois$elm_mdl$Material_Options$cs('mdl-layout__header--waterfall');
					}
			}
		}();
		return A3(
			_debois$elm_mdl$Material_Options$styled,
			_elm_lang$html$Html$header,
			{
				ctor: '::',
				_0: _debois$elm_mdl$Material_Options$cs('mdl-layout__header'),
				_1: {
					ctor: '::',
					_0: A2(
						_debois$elm_mdl$Material_Options$when,
						_elm_lang$core$Native_Utils.eq(config.mode, _debois$elm_mdl$Material_Layout$Standard) || (_debois$elm_mdl$Material_Layout$isWaterfall(config.mode) && model.isCompact),
						_debois$elm_mdl$Material_Options$cs('is-casting-shadow')),
					_1: {
						ctor: '::',
						_0: A2(
							_debois$elm_mdl$Material_Options$when,
							model.isAnimating,
							_debois$elm_mdl$Material_Options$cs('is-animating')),
						_1: {
							ctor: '::',
							_0: A2(
								_debois$elm_mdl$Material_Options$when,
								model.isCompact,
								_debois$elm_mdl$Material_Options$cs('is-compact')),
							_1: {
								ctor: '::',
								_0: mode,
								_1: {
									ctor: '::',
									_0: A2(
										_debois$elm_mdl$Material_Options$when,
										config.transparentHeader,
										_debois$elm_mdl$Material_Options$cs('mdl-layout__header--transparent')),
									_1: {
										ctor: '::',
										_0: _debois$elm_mdl$Material_Options$onClick(
											lift(
												_debois$elm_mdl$Material_Layout$TransitionHeader(
													{toCompact: false, fixedHeader: config.fixedHeader}))),
										_1: {
											ctor: '::',
											_0: A2(
												_debois$elm_mdl$Material_Options$on,
												'transitionend',
												_elm_lang$core$Json_Decode$succeed(
													lift(_debois$elm_mdl$Material_Layout$TransitionEnd))),
											_1: {ctor: '[]'}
										}
									}
								}
							}
						}
					}
				}
			},
			A2(
				_elm_lang$core$List$concatMap,
				function (x) {
					return x;
				},
				{
					ctor: '::',
					_0: _debois$elm_mdl$Material_Layout$toList(_p21._0),
					_1: {
						ctor: '::',
						_0: _p21._1,
						_1: {
							ctor: '::',
							_0: _debois$elm_mdl$Material_Layout$toList(_p21._2),
							_1: {ctor: '[]'}
						}
					}
				}));
	});
var _debois$elm_mdl$Material_Layout$Right = {ctor: 'Right'};
var _debois$elm_mdl$Material_Layout$Left = {ctor: 'Left'};
var _debois$elm_mdl$Material_Layout$tabsView = F4(
	function (lift, config, model, _p23) {
		var _p24 = _p23;
		var _p27 = _p24._1;
		var chevron = F2(
			function (direction, offset) {
				var dir = function () {
					var _p25 = direction;
					if (_p25.ctor === 'Left') {
						return 'left';
					} else {
						return 'right';
					}
				}();
				return A3(
					_debois$elm_mdl$Material_Options$styled,
					_elm_lang$html$Html$div,
					{
						ctor: '::',
						_0: _debois$elm_mdl$Material_Options$cs('mdl-layout__tab-bar-button'),
						_1: {
							ctor: '::',
							_0: _debois$elm_mdl$Material_Options$cs(
								A2(
									_elm_lang$core$Basics_ops['++'],
									'mdl-layout__tab-bar-',
									A2(_elm_lang$core$Basics_ops['++'], dir, '-button'))),
							_1: {
								ctor: '::',
								_0: A2(
									_debois$elm_mdl$Material_Options$when,
									(_elm_lang$core$Native_Utils.eq(direction, _debois$elm_mdl$Material_Layout$Left) && model.tabScrollState.canScrollLeft) || (_elm_lang$core$Native_Utils.eq(direction, _debois$elm_mdl$Material_Layout$Right) && model.tabScrollState.canScrollRight),
									_debois$elm_mdl$Material_Options$cs('is-active')),
								_1: {
									ctor: '::',
									_0: _debois$elm_mdl$Material_Options$many(_p27),
									_1: {ctor: '[]'}
								}
							}
						}
					},
					{
						ctor: '::',
						_0: A2(
							_debois$elm_mdl$Material_Icon$view,
							A2(_elm_lang$core$Basics_ops['++'], 'chevron_', dir),
							{
								ctor: '::',
								_0: _debois$elm_mdl$Material_Icon$size24,
								_1: {
									ctor: '::',
									_0: _debois$elm_mdl$Material_Options_Internal$attribute(
										A2(
											_elm_lang$html$Html_Attributes$attribute,
											'onclick',
											A2(
												_elm_lang$core$Basics_ops['++'],
												'document.getElementsByClassName(\'mdl-layout__tab-bar\')[0].scrollLeft += ',
												_elm_lang$core$Basics$toString(offset)))),
									_1: {ctor: '[]'}
								}
							}),
						_1: {ctor: '[]'}
					});
			});
		return A2(
			_debois$elm_mdl$Material_Options$div,
			{
				ctor: '::',
				_0: _debois$elm_mdl$Material_Options$cs('mdl-layout__tab-bar-container'),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: A2(chevron, _debois$elm_mdl$Material_Layout$Left, -100),
				_1: {
					ctor: '::',
					_0: A2(
						_debois$elm_mdl$Material_Options$div,
						{
							ctor: '::',
							_0: _debois$elm_mdl$Material_Options$cs('mdl-layout__tab-bar'),
							_1: {
								ctor: '::',
								_0: A2(_debois$elm_mdl$Material_Options$css, 'position', 'relative'),
								_1: {
									ctor: '::',
									_0: A2(_debois$elm_mdl$Material_Options$css, 'scroll-behavior', 'smooth'),
									_1: {
										ctor: '::',
										_0: config.rippleTabs ? _debois$elm_mdl$Material_Options$many(
											{
												ctor: '::',
												_0: _debois$elm_mdl$Material_Options$cs('mdl-js-ripple-effect'),
												_1: {
													ctor: '::',
													_0: _debois$elm_mdl$Material_Options$cs('mds-js-ripple-effect--ignore-events'),
													_1: {ctor: '[]'}
												}
											}) : _debois$elm_mdl$Material_Options$nop,
										_1: {
											ctor: '::',
											_0: _elm_lang$core$Native_Utils.eq(config.mode, _debois$elm_mdl$Material_Layout$Standard) ? _debois$elm_mdl$Material_Options$cs('is-casting-shadow') : _debois$elm_mdl$Material_Options$nop,
											_1: {
												ctor: '::',
												_0: _debois$elm_mdl$Material_Options$many(_p27),
												_1: {
													ctor: '::',
													_0: _debois$elm_mdl$Material_Options_Internal$attribute(
														A2(
															_elm_lang$html$Html_Events$on,
															'scroll',
															_debois$elm_dom$DOM$target(
																A4(
																	_elm_lang$core$Json_Decode$map3,
																	F3(
																		function (scrollWidth, clientWidth, scrollLeft) {
																			return lift(
																				_debois$elm_mdl$Material_Layout$ScrollTab(
																					{
																						canScrollLeft: _elm_lang$core$Native_Utils.cmp(scrollLeft, 0) > 0,
																						canScrollRight: _elm_lang$core$Native_Utils.cmp(scrollWidth - clientWidth, scrollLeft + 1) > 0,
																						width: _elm_lang$core$Maybe$Just(scrollWidth)
																					}));
																		}),
																	A2(_elm_lang$core$Json_Decode$field, 'scrollWidth', _elm_lang$core$Json_Decode$float),
																	A2(_elm_lang$core$Json_Decode$field, 'clientWidth', _elm_lang$core$Json_Decode$float),
																	A2(_elm_lang$core$Json_Decode$field, 'scrollLeft', _elm_lang$core$Json_Decode$float))))),
													_1: {ctor: '[]'}
												}
											}
										}
									}
								}
							}
						},
						A2(
							_elm_lang$core$List$indexedMap,
							F2(
								function (tabIndex, tab) {
									return A3(
										_debois$elm_mdl$Material_Helpers$filter,
										_elm_lang$html$Html$a,
										{
											ctor: '::',
											_0: _elm_lang$html$Html_Attributes$classList(
												{
													ctor: '::',
													_0: {ctor: '_Tuple2', _0: 'mdl-layout__tab', _1: true},
													_1: {
														ctor: '::',
														_0: {
															ctor: '_Tuple2',
															_0: 'is-active',
															_1: _elm_lang$core$Native_Utils.eq(tabIndex, config.selectedTab)
														},
														_1: {ctor: '[]'}
													}
												}),
											_1: {
												ctor: '::',
												_0: A2(
													_elm_lang$core$Maybe$withDefault,
													_debois$elm_mdl$Material_Helpers$noAttr,
													A2(
														_elm_lang$core$Maybe$map,
														F2(
															function (x, y) {
																return y(x);
															})(tabIndex),
														config.onSelectTab)),
												_1: {ctor: '[]'}
											}
										},
										{
											ctor: '::',
											_0: _elm_lang$core$Maybe$Just(tab),
											_1: {
												ctor: '::',
												_0: config.rippleTabs ? _elm_lang$core$Maybe$Just(
													A2(
														_elm_lang$html$Html$map,
														function (_p26) {
															return lift(
																A2(_debois$elm_mdl$Material_Layout$Ripple, tabIndex, _p26));
														},
														A2(
															_debois$elm_mdl$Material_Ripple$view,
															{
																ctor: '::',
																_0: _elm_lang$html$Html_Attributes$class('mdl-layout__tab-ripple-container'),
																_1: {ctor: '[]'}
															},
															A2(
																_elm_lang$core$Maybe$withDefault,
																_debois$elm_mdl$Material_Ripple$model,
																A2(_elm_lang$core$Dict$get, tabIndex, model.ripples))))) : _elm_lang$core$Maybe$Nothing,
												_1: {ctor: '[]'}
											}
										});
								}),
							_p24._0)),
					_1: {
						ctor: '::',
						_0: A2(chevron, _debois$elm_mdl$Material_Layout$Right, 100),
						_1: {ctor: '[]'}
					}
				}
			});
	});
var _debois$elm_mdl$Material_Layout$view = F4(
	function (lift, model, options, _p28) {
		var _p29 = _p28;
		var _p37 = _p29.tabs;
		var _p36 = _p29.header;
		var _p35 = _p29.drawer;
		var hasDrawer = !_elm_lang$core$Native_Utils.eq(
			_p35,
			{ctor: '[]'});
		var hasTabs = !_elm_lang$core$List$isEmpty(
			_elm_lang$core$Tuple$first(_p37));
		var hasHeader = hasTabs || (!_elm_lang$core$List$isEmpty(_p36));
		var summary = A2(_debois$elm_mdl$Material_Options_Internal$collect, _debois$elm_mdl$Material_Layout$defaultConfig, options);
		var config = summary.config;
		var drawerIsFixed = config.fixedDrawer && (!model.isSmallScreen);
		var drawerIsVisible = model.isDrawerOpen && (!drawerIsFixed);
		var _p30 = function () {
			var _p31 = {ctor: '_Tuple3', _0: _p35, _1: _p36, _2: config.fixedHeader};
			if ((_p31.ctor === '_Tuple3') && (_p31._0.ctor === '::')) {
				if ((_p31._1.ctor === '::') && (_p31._2 === true)) {
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Maybe$Nothing,
						_1: _elm_lang$core$Maybe$Just(
							A2(_debois$elm_mdl$Material_Layout$drawerButton, lift, drawerIsVisible))
					};
				} else {
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Maybe$Just(
							A2(_debois$elm_mdl$Material_Layout$drawerButton, lift, drawerIsVisible)),
						_1: _elm_lang$core$Maybe$Nothing
					};
				}
			} else {
				return {ctor: '_Tuple2', _0: _elm_lang$core$Maybe$Nothing, _1: _elm_lang$core$Maybe$Nothing};
			}
		}();
		var contentDrawerButton = _p30._0;
		var headerDrawerButton = _p30._1;
		var tabsElems = (!hasTabs) ? _elm_lang$core$Maybe$Nothing : _elm_lang$core$Maybe$Just(
			A4(_debois$elm_mdl$Material_Layout$tabsView, lift, config, model, _p37));
		return A2(
			_elm_lang$html$Html$div,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$classList(
					{
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: 'mdl-layout__container', _1: true},
						_1: {
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: 'has-scrolling-header',
								_1: _elm_lang$core$Native_Utils.eq(config.mode, _debois$elm_mdl$Material_Layout$Scrolling)
							},
							_1: {ctor: '[]'}
						}
					}),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: A3(
					_debois$elm_mdl$Material_Helpers$filter,
					_elm_lang$html$Html_Keyed$node('div'),
					A2(
						_elm_lang$core$List$filterMap,
						_elm_lang$core$Basics$identity,
						{
							ctor: '::',
							_0: _elm_lang$core$Maybe$Just(
								_elm_lang$html$Html_Attributes$classList(
									{
										ctor: '::',
										_0: {ctor: '_Tuple2', _0: 'mdl-layout ', _1: true},
										_1: {
											ctor: '::',
											_0: {ctor: '_Tuple2', _0: 'is-upgraded', _1: true},
											_1: {
												ctor: '::',
												_0: {ctor: '_Tuple2', _0: 'is-small-screen', _1: model.isSmallScreen},
												_1: {
													ctor: '::',
													_0: {ctor: '_Tuple2', _0: 'has-drawer', _1: hasDrawer},
													_1: {
														ctor: '::',
														_0: {ctor: '_Tuple2', _0: 'has-tabs', _1: hasTabs},
														_1: {
															ctor: '::',
															_0: {ctor: '_Tuple2', _0: 'mdl-js-layout', _1: true},
															_1: {
																ctor: '::',
																_0: {ctor: '_Tuple2', _0: 'mdl-layout--fixed-drawer', _1: config.fixedDrawer && hasDrawer},
																_1: {
																	ctor: '::',
																	_0: {ctor: '_Tuple2', _0: 'mdl-layout--fixed-header', _1: config.fixedHeader && hasHeader},
																	_1: {
																		ctor: '::',
																		_0: {ctor: '_Tuple2', _0: 'mdl-layout--fixed-tabs', _1: config.fixedTabs && hasTabs},
																		_1: {ctor: '[]'}
																	}
																}
															}
														}
													}
												}
											}
										}
									})),
							_1: {
								ctor: '::',
								_0: drawerIsVisible ? _elm_lang$core$Maybe$Just(
									A2(
										_elm_lang$html$Html_Events$on,
										'keydown',
										A2(
											_elm_lang$core$Json_Decode$map,
											function (_p32) {
												return lift(
													function (key) {
														return _elm_lang$core$Native_Utils.eq(key, 27) ? _debois$elm_mdl$Material_Layout$ToggleDrawer : _debois$elm_mdl$Material_Layout$NOP;
													}(_p32));
											},
											_elm_lang$html$Html_Events$keyCode))) : _elm_lang$core$Maybe$Nothing,
								_1: {ctor: '[]'}
							}
						}),
					{
						ctor: '::',
						_0: hasHeader ? _elm_lang$core$Maybe$Just(
							A2(
								F2(
									function (v0, v1) {
										return {ctor: '_Tuple2', _0: v0, _1: v1};
									}),
								'elm-mdl-header',
								A4(
									_debois$elm_mdl$Material_Layout$headerView,
									lift,
									config,
									model,
									{ctor: '_Tuple3', _0: headerDrawerButton, _1: _p36, _2: tabsElems}))) : _elm_lang$core$Maybe$Nothing,
						_1: {
							ctor: '::',
							_0: (!hasDrawer) ? _elm_lang$core$Maybe$Nothing : _elm_lang$core$Maybe$Just(
								{
									ctor: '_Tuple2',
									_0: 'elm-mdl-drawer',
									_1: A3(_debois$elm_mdl$Material_Layout$drawerView, lift, drawerIsVisible, _p35)
								}),
							_1: {
								ctor: '::',
								_0: (!hasDrawer) ? _elm_lang$core$Maybe$Nothing : _elm_lang$core$Maybe$Just(
									{
										ctor: '_Tuple2',
										_0: 'elm-mdl-obfuscator',
										_1: A2(_debois$elm_mdl$Material_Layout$obfuscator, lift, drawerIsVisible)
									}),
								_1: {
									ctor: '::',
									_0: A2(
										_elm_lang$core$Maybe$map,
										F2(
											function (v0, v1) {
												return {ctor: '_Tuple2', _0: v0, _1: v1};
											})('elm-drawer-button'),
										contentDrawerButton),
									_1: {
										ctor: '::',
										_0: _elm_lang$core$Maybe$Just(
											A2(
												F2(
													function (v0, v1) {
														return {ctor: '_Tuple2', _0: v0, _1: v1};
													}),
												_elm_lang$core$Basics$toString(config.selectedTab),
												A3(
													_debois$elm_mdl$Material_Options$styled,
													_elm_lang$html$Html$main_,
													{
														ctor: '::',
														_0: _debois$elm_mdl$Material_Options$id(_debois$elm_mdl$Material_Layout$mainId),
														_1: {
															ctor: '::',
															_0: _debois$elm_mdl$Material_Options$cs('mdl-layout__content'),
															_1: {
																ctor: '::',
																_0: A2(
																	_debois$elm_mdl$Material_Options$when,
																	_elm_lang$core$Native_Utils.eq(config.mode, _debois$elm_mdl$Material_Layout$Scrolling) && config.fixedHeader,
																	A2(_debois$elm_mdl$Material_Options$css, 'overflow-y', 'visible')),
																_1: {
																	ctor: '::',
																	_0: A2(
																		_debois$elm_mdl$Material_Options$when,
																		_elm_lang$core$Native_Utils.eq(config.mode, _debois$elm_mdl$Material_Layout$Scrolling) && config.fixedHeader,
																		A2(_debois$elm_mdl$Material_Options$css, 'overflow-x', 'visible')),
																	_1: {
																		ctor: '::',
																		_0: A2(
																			_debois$elm_mdl$Material_Options$when,
																			_elm_lang$core$Native_Utils.eq(config.mode, _debois$elm_mdl$Material_Layout$Scrolling) && config.fixedHeader,
																			A2(_debois$elm_mdl$Material_Options$css, 'overflow', 'visible')),
																		_1: {
																			ctor: '::',
																			_0: A2(
																				_debois$elm_mdl$Material_Options$when,
																				_debois$elm_mdl$Material_Layout$isWaterfall(config.mode),
																				function (_p33) {
																					return _debois$elm_mdl$Material_Options_Internal$attribute(
																						A2(_elm_lang$html$Html_Events$on, 'scroll', _p33));
																				}(
																					A2(
																						_elm_lang$core$Json_Decode$map,
																						function (_p34) {
																							return lift(
																								A2(_debois$elm_mdl$Material_Layout$ScrollPane, config.fixedHeader, _p34));
																						},
																						_debois$elm_dom$DOM$target(_debois$elm_dom$DOM$scrollTop)))),
																			_1: {ctor: '[]'}
																		}
																	}
																}
															}
														}
													},
													_p29.main))),
										_1: {ctor: '[]'}
									}
								}
							}
						}
					}),
				_1: {ctor: '[]'}
			});
	});
var _debois$elm_mdl$Material_Layout$render = A3(_debois$elm_mdl$Material_Component$render1, _debois$elm_mdl$Material_Layout$get, _debois$elm_mdl$Material_Layout$view, _debois$elm_mdl$Material_Component$LayoutMsg);

var _debois$elm_mdl$Material_Toggles$group = function (_p0) {
	return _debois$elm_mdl$Material_Options$attribute(
		_elm_lang$html$Html_Attributes$name(_p0));
};
var _debois$elm_mdl$Material_Toggles$value = function (_p1) {
	return _debois$elm_mdl$Material_Options_Internal$option(
		F2(
			function (b, options) {
				return _elm_lang$core$Native_Utils.update(
					options,
					{value: b});
			})(_p1));
};
var _debois$elm_mdl$Material_Toggles$disabled = _debois$elm_mdl$Material_Options_Internal$attribute(
	_elm_lang$html$Html_Attributes$disabled(true));
var _debois$elm_mdl$Material_Toggles$ripple = _debois$elm_mdl$Material_Options_Internal$option(
	function (options) {
		return _elm_lang$core$Native_Utils.update(
			options,
			{ripple: true});
	});
var _debois$elm_mdl$Material_Toggles$defaultConfig = {
	value: false,
	ripple: false,
	input: {ctor: '[]'},
	container: {ctor: '[]'}
};
var _debois$elm_mdl$Material_Toggles$defaultModel = {ripple: _debois$elm_mdl$Material_Ripple$model, isFocused: false};
var _debois$elm_mdl$Material_Toggles$_p2 = A3(
	_debois$elm_mdl$Material_Component$indexed,
	function (_) {
		return _.toggles;
	},
	F2(
		function (x, y) {
			return _elm_lang$core$Native_Utils.update(
				y,
				{toggles: x});
		}),
	_debois$elm_mdl$Material_Toggles$defaultModel);
var _debois$elm_mdl$Material_Toggles$get = _debois$elm_mdl$Material_Toggles$_p2._0;
var _debois$elm_mdl$Material_Toggles$set = _debois$elm_mdl$Material_Toggles$_p2._1;
var _debois$elm_mdl$Material_Toggles$Model = F2(
	function (a, b) {
		return {ripple: a, isFocused: b};
	});
var _debois$elm_mdl$Material_Toggles$Config = F4(
	function (a, b, c, d) {
		return {value: a, ripple: b, input: c, container: d};
	});
var _debois$elm_mdl$Material_Toggles$SetFocus = function (a) {
	return {ctor: 'SetFocus', _0: a};
};
var _debois$elm_mdl$Material_Toggles$Ripple = function (a) {
	return {ctor: 'Ripple', _0: a};
};
var _debois$elm_mdl$Material_Toggles$update = F2(
	function (action, model) {
		var _p3 = action;
		if (_p3.ctor === 'Ripple') {
			return A2(
				_debois$elm_mdl$Material_Helpers$map2nd,
				_elm_lang$core$Platform_Cmd$map(_debois$elm_mdl$Material_Toggles$Ripple),
				A2(
					_debois$elm_mdl$Material_Helpers$map1st,
					function (r) {
						return _elm_lang$core$Native_Utils.update(
							model,
							{ripple: r});
					},
					A2(_debois$elm_mdl$Material_Ripple$update, _p3._0, model.ripple)));
		} else {
			return {
				ctor: '_Tuple2',
				_0: _elm_lang$core$Native_Utils.update(
					model,
					{isFocused: _p3._0}),
				_1: _elm_lang$core$Platform_Cmd$none
			};
		}
	});
var _debois$elm_mdl$Material_Toggles$react = A4(
	_debois$elm_mdl$Material_Component$react,
	_debois$elm_mdl$Material_Toggles$get,
	_debois$elm_mdl$Material_Toggles$set,
	_debois$elm_mdl$Material_Component$TogglesMsg,
	_debois$elm_mdl$Material_Component$generalise(_debois$elm_mdl$Material_Toggles$update));
var _debois$elm_mdl$Material_Toggles$top = F5(
	function (lift, kind, model, summary, elems) {
		var cfg = summary.config;
		return A4(
			_debois$elm_mdl$Material_Options_Internal$applyContainer,
			summary,
			_elm_lang$html$Html$label,
			{
				ctor: '::',
				_0: _debois$elm_mdl$Material_Options$cs(
					A2(_elm_lang$core$Basics_ops['++'], 'mdl-', kind)),
				_1: {
					ctor: '::',
					_0: _debois$elm_mdl$Material_Options$cs(
						A2(_elm_lang$core$Basics_ops['++'], 'mdl-js-', kind)),
					_1: {
						ctor: '::',
						_0: A2(
							_debois$elm_mdl$Material_Options$when,
							cfg.ripple,
							_debois$elm_mdl$Material_Options$cs('mdl-js-ripple-effect')),
						_1: {
							ctor: '::',
							_0: A2(
								_debois$elm_mdl$Material_Options$when,
								cfg.ripple,
								_debois$elm_mdl$Material_Options$cs('mdl-js-ripple-effect--ignore-events')),
							_1: {
								ctor: '::',
								_0: _debois$elm_mdl$Material_Options$cs('is-upgraded'),
								_1: {
									ctor: '::',
									_0: A2(
										_debois$elm_mdl$Material_Options$when,
										cfg.value,
										_debois$elm_mdl$Material_Options$cs('is-checked')),
									_1: {
										ctor: '::',
										_0: A2(
											_debois$elm_mdl$Material_Options$when,
											model.isFocused,
											_debois$elm_mdl$Material_Options$cs('is-focused')),
										_1: {
											ctor: '::',
											_0: A3(
												_debois$elm_mdl$Material_Options_Internal$on1,
												'focus',
												lift,
												_debois$elm_mdl$Material_Toggles$SetFocus(true)),
											_1: {
												ctor: '::',
												_0: A3(
													_debois$elm_mdl$Material_Options_Internal$on1,
													'blur',
													lift,
													_debois$elm_mdl$Material_Toggles$SetFocus(false)),
												_1: {
													ctor: '::',
													_0: _debois$elm_mdl$Material_Options_Internal$attribute(
														_debois$elm_mdl$Material_Helpers$blurOn('mouseup')),
													_1: {ctor: '[]'}
												}
											}
										}
									}
								}
							}
						}
					}
				}
			},
			_elm_lang$core$List$concat(
				{
					ctor: '::',
					_0: elems,
					_1: {
						ctor: '::',
						_0: cfg.ripple ? {
							ctor: '::',
							_0: A2(
								_elm_lang$html$Html$map,
								function (_p4) {
									return lift(
										_debois$elm_mdl$Material_Toggles$Ripple(_p4));
								},
								A2(
									_debois$elm_mdl$Material_Ripple$view,
									{
										ctor: '::',
										_0: _elm_lang$html$Html_Attributes$class('mdl-switch__ripple-container mdl-js-ripple-effect mdl-ripple--center'),
										_1: {ctor: '[]'}
									},
									model.ripple)),
							_1: {ctor: '[]'}
						} : {ctor: '[]'},
						_1: {ctor: '[]'}
					}
				}));
	});
var _debois$elm_mdl$Material_Toggles$viewCheckbox = F4(
	function (lift, model, config, elems) {
		var summary = A2(_debois$elm_mdl$Material_Options_Internal$collect, _debois$elm_mdl$Material_Toggles$defaultConfig, config);
		return A5(
			_debois$elm_mdl$Material_Toggles$top,
			lift,
			'checkbox',
			model,
			summary,
			{
				ctor: '::',
				_0: A4(
					_debois$elm_mdl$Material_Options_Internal$applyInput,
					summary,
					_elm_lang$html$Html$input,
					{
						ctor: '::',
						_0: _debois$elm_mdl$Material_Options$cs('mdl-checkbox__input'),
						_1: {
							ctor: '::',
							_0: _debois$elm_mdl$Material_Options_Internal$attribute(
								_elm_lang$html$Html_Attributes$type_('checkbox')),
							_1: {
								ctor: '::',
								_0: _debois$elm_mdl$Material_Options_Internal$attribute(
									_elm_lang$html$Html_Attributes$checked(summary.config.value)),
								_1: {ctor: '[]'}
							}
						}
					},
					{ctor: '[]'}),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$span,
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$class('mdl-checkbox__label'),
							_1: {ctor: '[]'}
						},
						elems),
					_1: {
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$span,
							{
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$class('mdl-checkbox__focus-helper'),
								_1: {ctor: '[]'}
							},
							{ctor: '[]'}),
						_1: {
							ctor: '::',
							_0: A2(
								_elm_lang$html$Html$span,
								{
									ctor: '::',
									_0: _elm_lang$html$Html_Attributes$class('mdl-checkbox__box-outline'),
									_1: {ctor: '[]'}
								},
								{
									ctor: '::',
									_0: A2(
										_elm_lang$html$Html$span,
										{
											ctor: '::',
											_0: _elm_lang$html$Html_Attributes$class('mdl-checkbox__tick-outline'),
											_1: {ctor: '[]'}
										},
										{ctor: '[]'}),
									_1: {ctor: '[]'}
								}),
							_1: {ctor: '[]'}
						}
					}
				}
			});
	});
var _debois$elm_mdl$Material_Toggles$checkbox = A3(_debois$elm_mdl$Material_Component$render, _debois$elm_mdl$Material_Toggles$get, _debois$elm_mdl$Material_Toggles$viewCheckbox, _debois$elm_mdl$Material_Component$TogglesMsg);
var _debois$elm_mdl$Material_Toggles$viewSwitch = F4(
	function (lift, model, config, elems) {
		var summary = A2(_debois$elm_mdl$Material_Options_Internal$collect, _debois$elm_mdl$Material_Toggles$defaultConfig, config);
		return A5(
			_debois$elm_mdl$Material_Toggles$top,
			lift,
			'switch',
			model,
			summary,
			{
				ctor: '::',
				_0: A4(
					_debois$elm_mdl$Material_Options_Internal$applyInput,
					summary,
					_elm_lang$html$Html$input,
					{
						ctor: '::',
						_0: _debois$elm_mdl$Material_Options$cs('mdl-switch__input'),
						_1: {
							ctor: '::',
							_0: _debois$elm_mdl$Material_Options_Internal$attribute(
								_elm_lang$html$Html_Attributes$type_('checkbox')),
							_1: {
								ctor: '::',
								_0: _debois$elm_mdl$Material_Options_Internal$attribute(
									_elm_lang$html$Html_Attributes$checked(summary.config.value)),
								_1: {ctor: '[]'}
							}
						}
					},
					{ctor: '[]'}),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$span,
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$class('mdl-switch__label'),
							_1: {ctor: '[]'}
						},
						elems),
					_1: {
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$div,
							{
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$class('mdl-switch__track'),
								_1: {ctor: '[]'}
							},
							{ctor: '[]'}),
						_1: {
							ctor: '::',
							_0: A2(
								_elm_lang$html$Html$div,
								{
									ctor: '::',
									_0: _elm_lang$html$Html_Attributes$class('mdl-switch__thumb'),
									_1: {ctor: '[]'}
								},
								{
									ctor: '::',
									_0: A2(
										_elm_lang$html$Html$span,
										{
											ctor: '::',
											_0: _elm_lang$html$Html_Attributes$class('mdl-switch__focus-helper'),
											_1: {ctor: '[]'}
										},
										{ctor: '[]'}),
									_1: {ctor: '[]'}
								}),
							_1: {ctor: '[]'}
						}
					}
				}
			});
	});
var _debois$elm_mdl$Material_Toggles$switch = A3(_debois$elm_mdl$Material_Component$render, _debois$elm_mdl$Material_Toggles$get, _debois$elm_mdl$Material_Toggles$viewSwitch, _debois$elm_mdl$Material_Component$TogglesMsg);
var _debois$elm_mdl$Material_Toggles$viewRadio = F4(
	function (lift, model, config, elems) {
		var summary = A2(_debois$elm_mdl$Material_Options_Internal$collect, _debois$elm_mdl$Material_Toggles$defaultConfig, config);
		return A5(
			_debois$elm_mdl$Material_Toggles$top,
			lift,
			'radio',
			model,
			summary,
			{
				ctor: '::',
				_0: A4(
					_debois$elm_mdl$Material_Options_Internal$applyInput,
					summary,
					_elm_lang$html$Html$input,
					{
						ctor: '::',
						_0: _debois$elm_mdl$Material_Options$cs('mdl-radio__button'),
						_1: {
							ctor: '::',
							_0: _debois$elm_mdl$Material_Options$attribute(
								_elm_lang$html$Html_Attributes$type_('radio')),
							_1: {
								ctor: '::',
								_0: _debois$elm_mdl$Material_Options$attribute(
									_elm_lang$html$Html_Attributes$checked(summary.config.value)),
								_1: {ctor: '[]'}
							}
						}
					},
					{ctor: '[]'}),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$html$Html$span,
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$class('mdl-radio__label'),
							_1: {ctor: '[]'}
						},
						elems),
					_1: {
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$span,
							{
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$class('mdl-radio__outer-circle'),
								_1: {ctor: '[]'}
							},
							{ctor: '[]'}),
						_1: {
							ctor: '::',
							_0: A2(
								_elm_lang$html$Html$span,
								{
									ctor: '::',
									_0: _elm_lang$html$Html_Attributes$class('mdl-radio__inner-circle'),
									_1: {ctor: '[]'}
								},
								{ctor: '[]'}),
							_1: {ctor: '[]'}
						}
					}
				}
			});
	});
var _debois$elm_mdl$Material_Toggles$radio = A3(_debois$elm_mdl$Material_Component$render, _debois$elm_mdl$Material_Toggles$get, _debois$elm_mdl$Material_Toggles$viewRadio, _debois$elm_mdl$Material_Component$TogglesMsg);

var _debois$elm_mdl$Material_Tooltip$element = function (elem) {
	return _debois$elm_mdl$Material_Options_Internal$option(
		function (options) {
			return _elm_lang$core$Native_Utils.update(
				options,
				{elem: elem});
		});
};
var _debois$elm_mdl$Material_Tooltip$isTooltipClass = function (path) {
	return A2(
		_elm_lang$core$Json_Decode$andThen,
		function ($class) {
			return A2(_elm_lang$core$String$contains, 'mdl-tooltip', $class) ? _elm_lang$core$Json_Decode$succeed(true) : _elm_lang$core$Json_Decode$succeed(false);
		},
		A2(_elm_lang$core$Json_Decode$at, path, _debois$elm_dom$DOM$className));
};
var _debois$elm_mdl$Material_Tooltip$sibling = function (d) {
	var valid = function (path) {
		return A2(
			_elm_lang$core$Json_Decode$andThen,
			function (res) {
				return res ? A2(_elm_lang$core$Json_Decode$at, path, d) : _elm_lang$core$Json_Decode$fail('');
			},
			_debois$elm_mdl$Material_Tooltip$isTooltipClass(path));
	};
	var createPath = function (depth) {
		var parents = A2(_elm_lang$core$List$repeat, depth, 'parentElement');
		return A2(
			_elm_lang$core$Basics_ops['++'],
			{
				ctor: '::',
				_0: 'target',
				_1: {ctor: '[]'}
			},
			A2(
				_elm_lang$core$Basics_ops['++'],
				parents,
				{
					ctor: '::',
					_0: 'nextSibling',
					_1: {ctor: '[]'}
				}));
	};
	var paths = A2(
		_elm_lang$core$List$map,
		createPath,
		A2(_elm_lang$core$List$range, 0, 4));
	return _elm_lang$core$Json_Decode$oneOf(
		A2(_elm_lang$core$List$map, valid, paths));
};
var _debois$elm_mdl$Material_Tooltip$update = F2(
	function (action, model) {
		var _p0 = action;
		if (_p0.ctor === 'Enter') {
			return {
				ctor: '_Tuple2',
				_0: _elm_lang$core$Native_Utils.update(
					model,
					{isActive: true, domState: _p0._0}),
				_1: _elm_lang$core$Platform_Cmd$none
			};
		} else {
			return {
				ctor: '_Tuple2',
				_0: _elm_lang$core$Native_Utils.update(
					model,
					{isActive: false}),
				_1: _elm_lang$core$Platform_Cmd$none
			};
		}
	});
var _debois$elm_mdl$Material_Tooltip$calculatePos = F2(
	function (pos, domState) {
		var getValuesFor = F2(
			function (l, r) {
				return (_elm_lang$core$Native_Utils.cmp(l + r, 0) < 0) ? {ctor: '_Tuple2', _0: 0, _1: 0} : {ctor: '_Tuple2', _0: l, _1: r};
			});
		var offsetHeight = domState.offsetHeight;
		var marginTop = -1 * (offsetHeight / 2);
		var offsetWidth = domState.offsetWidth;
		var marginLeft = -1 * (offsetWidth / 2);
		var props = domState.rect;
		var left = props.left + (props.width / 2);
		var _p1 = A2(getValuesFor, left, marginLeft);
		var newLeft = _p1._0;
		var newMarginLeft = _p1._1;
		var top = props.top + (props.height / 2);
		var _p2 = A2(getValuesFor, top, marginTop);
		var newTop = _p2._0;
		var newMarginTop = _p2._1;
		var out = function () {
			var _p3 = pos;
			switch (_p3.ctor) {
				case 'Left':
					return {left: (props.left - offsetWidth) - 10, top: newTop, marginTop: newMarginTop, marginLeft: 0};
				case 'Right':
					return {left: (props.left + props.width) + 10, top: newTop, marginTop: newMarginTop, marginLeft: 0};
				case 'Top':
					return {left: newLeft, top: (props.top - offsetHeight) - 10, marginTop: 0, marginLeft: newMarginLeft};
				default:
					return {left: newLeft, top: (props.top + props.height) + 10, marginTop: 0, marginLeft: newMarginLeft};
			}
		}();
		return out;
	});
var _debois$elm_mdl$Material_Tooltip$defaultDOMState = {
	rect: {left: 0, top: 0, width: 0, height: 0},
	offsetWidth: 0,
	offsetHeight: 0
};
var _debois$elm_mdl$Material_Tooltip$defaultPos = {left: 0, top: 0, marginLeft: 0, marginTop: 0};
var _debois$elm_mdl$Material_Tooltip$defaultModel = {isActive: false, domState: _debois$elm_mdl$Material_Tooltip$defaultDOMState};
var _debois$elm_mdl$Material_Tooltip$_p4 = A3(
	_debois$elm_mdl$Material_Component$indexed,
	function (_) {
		return _.tooltip;
	},
	F2(
		function (x, y) {
			return _elm_lang$core$Native_Utils.update(
				y,
				{tooltip: x});
		}),
	_debois$elm_mdl$Material_Tooltip$defaultModel);
var _debois$elm_mdl$Material_Tooltip$get = _debois$elm_mdl$Material_Tooltip$_p4._0;
var _debois$elm_mdl$Material_Tooltip$set = _debois$elm_mdl$Material_Tooltip$_p4._1;
var _debois$elm_mdl$Material_Tooltip$react = A4(
	_debois$elm_mdl$Material_Component$react,
	_debois$elm_mdl$Material_Tooltip$get,
	_debois$elm_mdl$Material_Tooltip$set,
	_debois$elm_mdl$Material_Component$TooltipMsg,
	_debois$elm_mdl$Material_Component$generalise(_debois$elm_mdl$Material_Tooltip$update));
var _debois$elm_mdl$Material_Tooltip$Model = F2(
	function (a, b) {
		return {isActive: a, domState: b};
	});
var _debois$elm_mdl$Material_Tooltip$Pos = F4(
	function (a, b, c, d) {
		return {left: a, top: b, marginLeft: c, marginTop: d};
	});
var _debois$elm_mdl$Material_Tooltip$DOMState = F3(
	function (a, b, c) {
		return {rect: a, offsetWidth: b, offsetHeight: c};
	});
var _debois$elm_mdl$Material_Tooltip$stateDecoder = A4(
	_elm_lang$core$Json_Decode$map3,
	_debois$elm_mdl$Material_Tooltip$DOMState,
	_debois$elm_dom$DOM$target(_debois$elm_dom$DOM$boundingClientRect),
	_debois$elm_mdl$Material_Tooltip$sibling(_debois$elm_dom$DOM$offsetWidth),
	_debois$elm_mdl$Material_Tooltip$sibling(_debois$elm_dom$DOM$offsetHeight));
var _debois$elm_mdl$Material_Tooltip$Config = F3(
	function (a, b, c) {
		return {size: a, position: b, elem: c};
	});
var _debois$elm_mdl$Material_Tooltip$Leave = {ctor: 'Leave'};
var _debois$elm_mdl$Material_Tooltip$onMouseLeave = F2(
	function (lift, idx) {
		return A2(
			_elm_lang$html$Html_Events$on,
			'mouseleave',
			_elm_lang$core$Json_Decode$succeed(
				lift(
					A2(_debois$elm_mdl$Material_Component$TooltipMsg, idx, _debois$elm_mdl$Material_Tooltip$Leave))));
	});
var _debois$elm_mdl$Material_Tooltip$onLeave = function (lift) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseleave',
		A2(
			_elm_lang$core$Json_Decode$map,
			lift,
			_elm_lang$core$Json_Decode$succeed(_debois$elm_mdl$Material_Tooltip$Leave)));
};
var _debois$elm_mdl$Material_Tooltip$Enter = function (a) {
	return {ctor: 'Enter', _0: a};
};
var _debois$elm_mdl$Material_Tooltip$onMouseEnter = F2(
	function (lift, idx) {
		return A2(
			_elm_lang$html$Html_Events$on,
			'mouseenter',
			A2(
				_elm_lang$core$Json_Decode$map,
				function (_p5) {
					return lift(
						A2(
							_debois$elm_mdl$Material_Component$TooltipMsg,
							idx,
							_debois$elm_mdl$Material_Tooltip$Enter(_p5)));
				},
				_debois$elm_mdl$Material_Tooltip$stateDecoder));
	});
var _debois$elm_mdl$Material_Tooltip$attach = F2(
	function (lift, index) {
		return _debois$elm_mdl$Material_Options$many(
			{
				ctor: '::',
				_0: _debois$elm_mdl$Material_Options_Internal$attribute(
					A2(_debois$elm_mdl$Material_Tooltip$onMouseEnter, lift, index)),
				_1: {
					ctor: '::',
					_0: _debois$elm_mdl$Material_Options_Internal$attribute(
						A2(_debois$elm_mdl$Material_Tooltip$onMouseLeave, lift, index)),
					_1: {ctor: '[]'}
				}
			});
	});
var _debois$elm_mdl$Material_Tooltip$onEnter = function (lift) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseenter',
		A2(
			_elm_lang$core$Json_Decode$map,
			lift,
			A2(_elm_lang$core$Json_Decode$map, _debois$elm_mdl$Material_Tooltip$Enter, _debois$elm_mdl$Material_Tooltip$stateDecoder)));
};
var _debois$elm_mdl$Material_Tooltip$Large = {ctor: 'Large'};
var _debois$elm_mdl$Material_Tooltip$large = _debois$elm_mdl$Material_Options_Internal$option(
	function (options) {
		return _elm_lang$core$Native_Utils.update(
			options,
			{size: _debois$elm_mdl$Material_Tooltip$Large});
	});
var _debois$elm_mdl$Material_Tooltip$Default = {ctor: 'Default'};
var _debois$elm_mdl$Material_Tooltip$Bottom = {ctor: 'Bottom'};
var _debois$elm_mdl$Material_Tooltip$defaultConfig = {size: _debois$elm_mdl$Material_Tooltip$Default, position: _debois$elm_mdl$Material_Tooltip$Bottom, elem: _elm_lang$html$Html$div};
var _debois$elm_mdl$Material_Tooltip$view = F4(
	function (lift, model, options, content) {
		var px = function (f) {
			return A2(
				_elm_lang$core$Basics_ops['++'],
				_elm_lang$core$Basics$toString(f),
				'px');
		};
		var summary = A2(_debois$elm_mdl$Material_Options_Internal$collect, _debois$elm_mdl$Material_Tooltip$defaultConfig, options);
		var config = summary.config;
		var pos = model.isActive ? A2(_debois$elm_mdl$Material_Tooltip$calculatePos, config.position, model.domState) : _debois$elm_mdl$Material_Tooltip$defaultPos;
		return A3(
			_debois$elm_mdl$Material_Options$styled,
			config.elem,
			{
				ctor: '::',
				_0: _debois$elm_mdl$Material_Options$cs('mdl-tooltip'),
				_1: {
					ctor: '::',
					_0: A2(
						_debois$elm_mdl$Material_Options$when,
						model.isActive,
						_debois$elm_mdl$Material_Options$cs('is-active')),
					_1: {
						ctor: '::',
						_0: A2(
							_debois$elm_mdl$Material_Options$when,
							_elm_lang$core$Native_Utils.eq(config.size, _debois$elm_mdl$Material_Tooltip$Large),
							_debois$elm_mdl$Material_Options$cs('mdl-tooltip--large')),
						_1: {
							ctor: '::',
							_0: A2(
								_debois$elm_mdl$Material_Options$when,
								model.isActive,
								A2(
									_debois$elm_mdl$Material_Options$css,
									'left',
									px(pos.left))),
							_1: {
								ctor: '::',
								_0: A2(
									_debois$elm_mdl$Material_Options$when,
									model.isActive,
									A2(
										_debois$elm_mdl$Material_Options$css,
										'margin-left',
										px(pos.marginLeft))),
								_1: {
									ctor: '::',
									_0: A2(
										_debois$elm_mdl$Material_Options$when,
										model.isActive,
										A2(
											_debois$elm_mdl$Material_Options$css,
											'top',
											px(pos.top))),
									_1: {
										ctor: '::',
										_0: A2(
											_debois$elm_mdl$Material_Options$when,
											model.isActive,
											A2(
												_debois$elm_mdl$Material_Options$css,
												'margin-top',
												px(pos.marginTop))),
										_1: {ctor: '[]'}
									}
								}
							}
						}
					}
				}
			},
			content);
	});
var _debois$elm_mdl$Material_Tooltip$render = A3(_debois$elm_mdl$Material_Component$render, _debois$elm_mdl$Material_Tooltip$get, _debois$elm_mdl$Material_Tooltip$view, _debois$elm_mdl$Material_Component$TooltipMsg);
var _debois$elm_mdl$Material_Tooltip$bottom = _debois$elm_mdl$Material_Options_Internal$option(
	function (options) {
		return _elm_lang$core$Native_Utils.update(
			options,
			{position: _debois$elm_mdl$Material_Tooltip$Bottom});
	});
var _debois$elm_mdl$Material_Tooltip$Top = {ctor: 'Top'};
var _debois$elm_mdl$Material_Tooltip$top = _debois$elm_mdl$Material_Options_Internal$option(
	function (options) {
		return _elm_lang$core$Native_Utils.update(
			options,
			{position: _debois$elm_mdl$Material_Tooltip$Top});
	});
var _debois$elm_mdl$Material_Tooltip$Right = {ctor: 'Right'};
var _debois$elm_mdl$Material_Tooltip$right = _debois$elm_mdl$Material_Options_Internal$option(
	function (options) {
		return _elm_lang$core$Native_Utils.update(
			options,
			{position: _debois$elm_mdl$Material_Tooltip$Right});
	});
var _debois$elm_mdl$Material_Tooltip$Left = {ctor: 'Left'};
var _debois$elm_mdl$Material_Tooltip$left = _debois$elm_mdl$Material_Options_Internal$option(
	function (options) {
		return _elm_lang$core$Native_Utils.update(
			options,
			{position: _debois$elm_mdl$Material_Tooltip$Left});
	});

var _debois$elm_mdl$Material_Tabs$activeTab = function (_p0) {
	return _debois$elm_mdl$Material_Options_Internal$option(
		F2(
			function (k, config) {
				return _elm_lang$core$Native_Utils.update(
					config,
					{activeTab: k});
			})(_p0));
};
var _debois$elm_mdl$Material_Tabs$onSelectTab = function (_p1) {
	return _debois$elm_mdl$Material_Options_Internal$option(
		F2(
			function (k, config) {
				return _elm_lang$core$Native_Utils.update(
					config,
					{
						onSelectTab: _elm_lang$core$Maybe$Just(k)
					});
			})(_p1));
};
var _debois$elm_mdl$Material_Tabs$ripple = _debois$elm_mdl$Material_Options_Internal$option(
	function (config) {
		return _elm_lang$core$Native_Utils.update(
			config,
			{ripple: true});
	});
var _debois$elm_mdl$Material_Tabs$defaultConfig = {ripple: false, onSelectTab: _elm_lang$core$Maybe$Nothing, activeTab: 0};
var _debois$elm_mdl$Material_Tabs$defaultModel = {ripples: _elm_lang$core$Dict$empty};
var _debois$elm_mdl$Material_Tabs$_p2 = A3(
	_debois$elm_mdl$Material_Component$indexed,
	function (_) {
		return _.tabs;
	},
	F2(
		function (x, y) {
			return _elm_lang$core$Native_Utils.update(
				y,
				{tabs: x});
		}),
	_debois$elm_mdl$Material_Tabs$defaultModel);
var _debois$elm_mdl$Material_Tabs$get = _debois$elm_mdl$Material_Tabs$_p2._0;
var _debois$elm_mdl$Material_Tabs$set = _debois$elm_mdl$Material_Tabs$_p2._1;
var _debois$elm_mdl$Material_Tabs$Model = function (a) {
	return {ripples: a};
};
var _debois$elm_mdl$Material_Tabs$Config = F3(
	function (a, b, c) {
		return {ripple: a, onSelectTab: b, activeTab: c};
	});
var _debois$elm_mdl$Material_Tabs$Ripple = F2(
	function (a, b) {
		return {ctor: 'Ripple', _0: a, _1: b};
	});
var _debois$elm_mdl$Material_Tabs$update = F2(
	function (action, model) {
		var _p3 = action;
		var _p5 = _p3._0;
		var _p4 = A2(
			_debois$elm_mdl$Material_Ripple$update,
			_p3._1,
			A2(
				_elm_lang$core$Maybe$withDefault,
				_debois$elm_mdl$Material_Ripple$model,
				A2(_elm_lang$core$Dict$get, _p5, model.ripples)));
		var ripple_ = _p4._0;
		var cmd = _p4._1;
		return {
			ctor: '_Tuple2',
			_0: _elm_lang$core$Native_Utils.update(
				model,
				{
					ripples: A3(_elm_lang$core$Dict$insert, _p5, ripple_, model.ripples)
				}),
			_1: A2(
				_elm_lang$core$Platform_Cmd$map,
				_debois$elm_mdl$Material_Tabs$Ripple(_p5),
				cmd)
		};
	});
var _debois$elm_mdl$Material_Tabs$react = A4(
	_debois$elm_mdl$Material_Component$react,
	_debois$elm_mdl$Material_Tabs$get,
	_debois$elm_mdl$Material_Tabs$set,
	_debois$elm_mdl$Material_Component$TabsMsg,
	_debois$elm_mdl$Material_Component$generalise(_debois$elm_mdl$Material_Tabs$update));
var _debois$elm_mdl$Material_Tabs$view = F5(
	function (lift, model, options, tabs, tabContent) {
		var wrapContent = A2(
			_elm_lang$html$Html_Keyed$node,
			'div',
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$classList(
					{
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: 'mdl-tab__panel', _1: true},
						_1: {
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'is-active', _1: true},
							_1: {ctor: '[]'}
						}
					}),
				_1: {ctor: '[]'}
			});
		var summary = A2(_debois$elm_mdl$Material_Options_Internal$collect, _debois$elm_mdl$Material_Tabs$defaultConfig, options);
		var config = summary.config;
		var unwrapLabel = F2(
			function (tabIdx, _p6) {
				var _p7 = _p6;
				var _p9 = _p7._0._1;
				return A3(
					_debois$elm_mdl$Material_Options$styled,
					_elm_lang$html$Html$a,
					{
						ctor: '::',
						_0: _debois$elm_mdl$Material_Options$cs('mdl-tabs__tab'),
						_1: {
							ctor: '::',
							_0: A2(
								_debois$elm_mdl$Material_Options$when,
								_elm_lang$core$Native_Utils.eq(tabIdx, config.activeTab),
								_debois$elm_mdl$Material_Options$cs('is-active')),
							_1: {
								ctor: '::',
								_0: A2(
									_elm_lang$core$Maybe$withDefault,
									_debois$elm_mdl$Material_Options$nop,
									A2(
										_elm_lang$core$Maybe$map,
										function (t) {
											return _debois$elm_mdl$Material_Options$onClick(
												t(tabIdx));
										},
										config.onSelectTab)),
								_1: {
									ctor: '::',
									_0: _debois$elm_mdl$Material_Options$many(_p7._0._0),
									_1: {ctor: '[]'}
								}
							}
						}
					},
					config.ripple ? _elm_lang$core$List$concat(
						{
							ctor: '::',
							_0: _p9,
							_1: {
								ctor: '::',
								_0: {
									ctor: '::',
									_0: A2(
										_elm_lang$html$Html$map,
										function (_p8) {
											return lift(
												A2(_debois$elm_mdl$Material_Tabs$Ripple, tabIdx, _p8));
										},
										A2(
											_debois$elm_mdl$Material_Ripple$view,
											{
												ctor: '::',
												_0: _elm_lang$html$Html_Attributes$classList(
													{
														ctor: '::',
														_0: {ctor: '_Tuple2', _0: 'mdl-tabs__ripple-container', _1: true},
														_1: {
															ctor: '::',
															_0: {ctor: '_Tuple2', _0: 'mdl-tabs__ripple-js-effect', _1: true},
															_1: {ctor: '[]'}
														}
													}),
												_1: {ctor: '[]'}
											},
											A2(
												_elm_lang$core$Maybe$withDefault,
												_debois$elm_mdl$Material_Ripple$model,
												A2(_elm_lang$core$Dict$get, tabIdx, model.ripples)))),
									_1: {ctor: '[]'}
								},
								_1: {ctor: '[]'}
							}
						}) : _p9);
			});
		var links = A3(
			_debois$elm_mdl$Material_Options$styled,
			_elm_lang$html$Html$div,
			{
				ctor: '::',
				_0: _debois$elm_mdl$Material_Options$cs('mdl-tabs__tab-bar'),
				_1: {ctor: '[]'}
			},
			A2(_elm_lang$core$List$indexedMap, unwrapLabel, tabs));
		return A5(
			_debois$elm_mdl$Material_Options_Internal$apply,
			summary,
			_elm_lang$html$Html$div,
			{
				ctor: '::',
				_0: _debois$elm_mdl$Material_Options$cs('mdl-tabs'),
				_1: {
					ctor: '::',
					_0: _debois$elm_mdl$Material_Options$cs('mdl-js-tabs'),
					_1: {
						ctor: '::',
						_0: _debois$elm_mdl$Material_Options$cs('is-upgraded'),
						_1: {
							ctor: '::',
							_0: A2(
								_debois$elm_mdl$Material_Options$when,
								config.ripple,
								_debois$elm_mdl$Material_Options$cs('mdl-js-ripple-effect')),
							_1: {
								ctor: '::',
								_0: A2(
									_debois$elm_mdl$Material_Options$when,
									config.ripple,
									_debois$elm_mdl$Material_Options$cs('mdl-js-ripple-effect--ignore-events')),
								_1: {ctor: '[]'}
							}
						}
					}
				}
			},
			{ctor: '[]'},
			{
				ctor: '::',
				_0: links,
				_1: {
					ctor: '::',
					_0: wrapContent(
						{
							ctor: '::',
							_0: {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Basics$toString(config.activeTab),
								_1: A2(
									_elm_lang$html$Html$div,
									{ctor: '[]'},
									tabContent)
							},
							_1: {ctor: '[]'}
						}),
					_1: {ctor: '[]'}
				}
			});
	});
var _debois$elm_mdl$Material_Tabs$render = A3(_debois$elm_mdl$Material_Component$render, _debois$elm_mdl$Material_Tabs$get, _debois$elm_mdl$Material_Tabs$view, _debois$elm_mdl$Material_Component$TabsMsg);
var _debois$elm_mdl$Material_Tabs$Label = function (a) {
	return {ctor: 'Label', _0: a};
};
var _debois$elm_mdl$Material_Tabs$label = F2(
	function (p, c) {
		return _debois$elm_mdl$Material_Tabs$Label(
			{ctor: '_Tuple2', _0: p, _1: c});
	});
var _debois$elm_mdl$Material_Tabs$textLabel = F2(
	function (p, c) {
		return A2(
			_debois$elm_mdl$Material_Tabs$label,
			p,
			{
				ctor: '::',
				_0: _elm_lang$html$Html$text(c),
				_1: {ctor: '[]'}
			});
	});

var _debois$elm_mdl$Material$init = function (lift) {
	return _debois$elm_mdl$Material_Layout$sub0(lift);
};
var _debois$elm_mdl$Material$subscriptions = F2(
	function (lift, model) {
		return _elm_lang$core$Platform_Sub$batch(
			{
				ctor: '::',
				_0: A2(_debois$elm_mdl$Material_Layout$subs, lift, model.mdl),
				_1: {
					ctor: '::',
					_0: A2(_debois$elm_mdl$Material_Menu$subs, lift, model.mdl),
					_1: {ctor: '[]'}
				}
			});
	});
var _debois$elm_mdl$Material$update_ = F3(
	function (lift, msg, store) {
		var _p0 = msg;
		switch (_p0.ctor) {
			case 'ButtonMsg':
				return A4(_debois$elm_mdl$Material_Button$react, lift, _p0._1, _p0._0, store);
			case 'TextfieldMsg':
				return A4(_debois$elm_mdl$Material_Textfield$react, lift, _p0._1, _p0._0, store);
			case 'MenuMsg':
				var _p2 = _p0._0;
				return A4(
					_debois$elm_mdl$Material_Menu$react,
					function (_p1) {
						return lift(
							A2(_debois$elm_mdl$Material_Component$MenuMsg, _p2, _p1));
					},
					_p0._1,
					_p2,
					store);
			case 'LayoutMsg':
				return A3(
					_debois$elm_mdl$Material_Layout$react,
					function (_p3) {
						return lift(
							_debois$elm_mdl$Material_Component$LayoutMsg(_p3));
					},
					_p0._0,
					store);
			case 'TogglesMsg':
				return A4(_debois$elm_mdl$Material_Toggles$react, lift, _p0._1, _p0._0, store);
			case 'TooltipMsg':
				return A4(_debois$elm_mdl$Material_Tooltip$react, lift, _p0._1, _p0._0, store);
			case 'TabsMsg':
				return A4(_debois$elm_mdl$Material_Tabs$react, lift, _p0._1, _p0._0, store);
			default:
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Maybe$Nothing,
					_1: _debois$elm_mdl$Material_Dispatch$forward(_p0._0)
				};
		}
	});
var _debois$elm_mdl$Material$update = F3(
	function (lift, msg, container) {
		return A2(
			_debois$elm_mdl$Material_Helpers$map1st,
			_elm_lang$core$Maybe$withDefault(container),
			A2(
				_debois$elm_mdl$Material_Helpers$map1st,
				_elm_lang$core$Maybe$map(
					function (mdl) {
						return _elm_lang$core$Native_Utils.update(
							container,
							{mdl: mdl});
					}),
				A3(
					_debois$elm_mdl$Material$update_,
					lift,
					msg,
					function (_) {
						return _.mdl;
					}(container))));
	});
var _debois$elm_mdl$Material$model = {button: _elm_lang$core$Dict$empty, textfield: _elm_lang$core$Dict$empty, menu: _elm_lang$core$Dict$empty, snackbar: _elm_lang$core$Maybe$Nothing, layout: _debois$elm_mdl$Material_Layout$defaultModel, toggles: _elm_lang$core$Dict$empty, tooltip: _elm_lang$core$Dict$empty, tabs: _elm_lang$core$Dict$empty};
var _debois$elm_mdl$Material$Model = F8(
	function (a, b, c, d, e, f, g, h) {
		return {button: a, textfield: b, menu: c, snackbar: d, layout: e, toggles: f, tooltip: g, tabs: h};
	});

var _debois$elm_mdl$Material_Badge$add = function (str) {
	return _debois$elm_mdl$Material_Options$many(
		{
			ctor: '::',
			_0: A2(_debois$elm_mdl$Material_Options$data, 'badge', str),
			_1: {
				ctor: '::',
				_0: _debois$elm_mdl$Material_Options$cs('mdl-badge'),
				_1: {ctor: '[]'}
			}
		});
};
var _debois$elm_mdl$Material_Badge$overlap = _debois$elm_mdl$Material_Options$cs('mdl-badge--overlap');
var _debois$elm_mdl$Material_Badge$noBackground = _debois$elm_mdl$Material_Options$cs('mdl-badge--no-background');

var _debois$elm_mdl$Material_Color$text = function (_p0) {
	var _p1 = _p0;
	return _debois$elm_mdl$Material_Options$cs(
		A2(_elm_lang$core$Basics_ops['++'], 'mdl-color-text--', _p1._0));
};
var _debois$elm_mdl$Material_Color$background = function (_p2) {
	var _p3 = _p2;
	return _debois$elm_mdl$Material_Options$cs(
		A2(_elm_lang$core$Basics_ops['++'], 'mdl-color--', _p3._0));
};
var _debois$elm_mdl$Material_Color$shadeName = function (shade) {
	var _p4 = shade;
	switch (_p4.ctor) {
		case 'S50':
			return '50';
		case 'S100':
			return '100';
		case 'S200':
			return '200';
		case 'S300':
			return '300';
		case 'S400':
			return '400';
		case 'S500':
			return '500';
		case 'S600':
			return '600';
		case 'S700':
			return '700';
		case 'S800':
			return '800';
		case 'S900':
			return '900';
		case 'A100':
			return 'A100';
		case 'A200':
			return 'A200';
		case 'A400':
			return 'A400';
		default:
			return 'A700';
	}
};
var _debois$elm_mdl$Material_Color$hueName = function (color) {
	var _p5 = color;
	switch (_p5.ctor) {
		case 'Indigo':
			return 'indigo';
		case 'Blue':
			return 'blue';
		case 'LightBlue':
			return 'light-blue';
		case 'Cyan':
			return 'cyan';
		case 'Teal':
			return 'teal';
		case 'Green':
			return 'green';
		case 'LightGreen':
			return 'light-green';
		case 'Lime':
			return 'lime';
		case 'Yellow':
			return 'yellow';
		case 'Amber':
			return 'amber';
		case 'Orange':
			return 'orange';
		case 'Brown':
			return 'brown';
		case 'BlueGrey':
			return 'blue-grey';
		case 'Grey':
			return 'grey';
		case 'DeepOrange':
			return 'deep-orange';
		case 'Red':
			return 'red';
		case 'Pink':
			return 'pink';
		case 'Purple':
			return 'purple';
		default:
			return 'deep-purple';
	}
};
var _debois$elm_mdl$Material_Color$scheme = F2(
	function (primary, accent) {
		var q = _elm_lang$core$String$map(
			function (x) {
				return _elm_lang$core$Native_Utils.eq(
					x,
					_elm_lang$core$Native_Utils.chr('-')) ? _elm_lang$core$Native_Utils.chr('_') : x;
			});
		var cssFile = function () {
			var _p6 = accent;
			switch (_p6.ctor) {
				case 'Grey':
					return '';
				case 'Brown':
					return '';
				case 'BlueGrey':
					return '';
				default:
					return A2(
						_elm_lang$core$Basics_ops['++'],
						'.',
						A2(
							_elm_lang$core$Basics_ops['++'],
							q(
								_debois$elm_mdl$Material_Color$hueName(primary)),
							A2(
								_elm_lang$core$Basics_ops['++'],
								'-',
								q(
									_debois$elm_mdl$Material_Color$hueName(accent)))));
			}
		}();
		return A2(
			_elm_lang$core$Basics_ops['++'],
			'material',
			A2(_elm_lang$core$Basics_ops['++'], cssFile, '.min.css'));
	});
var _debois$elm_mdl$Material_Color$DeepPurple = {ctor: 'DeepPurple'};
var _debois$elm_mdl$Material_Color$Purple = {ctor: 'Purple'};
var _debois$elm_mdl$Material_Color$Pink = {ctor: 'Pink'};
var _debois$elm_mdl$Material_Color$Red = {ctor: 'Red'};
var _debois$elm_mdl$Material_Color$DeepOrange = {ctor: 'DeepOrange'};
var _debois$elm_mdl$Material_Color$Grey = {ctor: 'Grey'};
var _debois$elm_mdl$Material_Color$BlueGrey = {ctor: 'BlueGrey'};
var _debois$elm_mdl$Material_Color$Brown = {ctor: 'Brown'};
var _debois$elm_mdl$Material_Color$Orange = {ctor: 'Orange'};
var _debois$elm_mdl$Material_Color$Amber = {ctor: 'Amber'};
var _debois$elm_mdl$Material_Color$Yellow = {ctor: 'Yellow'};
var _debois$elm_mdl$Material_Color$Lime = {ctor: 'Lime'};
var _debois$elm_mdl$Material_Color$LightGreen = {ctor: 'LightGreen'};
var _debois$elm_mdl$Material_Color$Green = {ctor: 'Green'};
var _debois$elm_mdl$Material_Color$Teal = {ctor: 'Teal'};
var _debois$elm_mdl$Material_Color$Cyan = {ctor: 'Cyan'};
var _debois$elm_mdl$Material_Color$LightBlue = {ctor: 'LightBlue'};
var _debois$elm_mdl$Material_Color$Blue = {ctor: 'Blue'};
var _debois$elm_mdl$Material_Color$Indigo = {ctor: 'Indigo'};
var _debois$elm_mdl$Material_Color$hues = _elm_lang$core$Array$fromList(
	{
		ctor: '::',
		_0: _debois$elm_mdl$Material_Color$Indigo,
		_1: {
			ctor: '::',
			_0: _debois$elm_mdl$Material_Color$Blue,
			_1: {
				ctor: '::',
				_0: _debois$elm_mdl$Material_Color$LightBlue,
				_1: {
					ctor: '::',
					_0: _debois$elm_mdl$Material_Color$Cyan,
					_1: {
						ctor: '::',
						_0: _debois$elm_mdl$Material_Color$Teal,
						_1: {
							ctor: '::',
							_0: _debois$elm_mdl$Material_Color$Green,
							_1: {
								ctor: '::',
								_0: _debois$elm_mdl$Material_Color$LightGreen,
								_1: {
									ctor: '::',
									_0: _debois$elm_mdl$Material_Color$Lime,
									_1: {
										ctor: '::',
										_0: _debois$elm_mdl$Material_Color$Yellow,
										_1: {
											ctor: '::',
											_0: _debois$elm_mdl$Material_Color$Amber,
											_1: {
												ctor: '::',
												_0: _debois$elm_mdl$Material_Color$Orange,
												_1: {
													ctor: '::',
													_0: _debois$elm_mdl$Material_Color$Brown,
													_1: {
														ctor: '::',
														_0: _debois$elm_mdl$Material_Color$BlueGrey,
														_1: {
															ctor: '::',
															_0: _debois$elm_mdl$Material_Color$Grey,
															_1: {
																ctor: '::',
																_0: _debois$elm_mdl$Material_Color$DeepOrange,
																_1: {
																	ctor: '::',
																	_0: _debois$elm_mdl$Material_Color$Red,
																	_1: {
																		ctor: '::',
																		_0: _debois$elm_mdl$Material_Color$Pink,
																		_1: {
																			ctor: '::',
																			_0: _debois$elm_mdl$Material_Color$Purple,
																			_1: {
																				ctor: '::',
																				_0: _debois$elm_mdl$Material_Color$DeepPurple,
																				_1: {ctor: '[]'}
																			}
																		}
																	}
																}
															}
														}
													}
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}
		}
	});
var _debois$elm_mdl$Material_Color$accentHues = _elm_lang$core$Array$fromList(
	{
		ctor: '::',
		_0: _debois$elm_mdl$Material_Color$Indigo,
		_1: {
			ctor: '::',
			_0: _debois$elm_mdl$Material_Color$Blue,
			_1: {
				ctor: '::',
				_0: _debois$elm_mdl$Material_Color$LightBlue,
				_1: {
					ctor: '::',
					_0: _debois$elm_mdl$Material_Color$Cyan,
					_1: {
						ctor: '::',
						_0: _debois$elm_mdl$Material_Color$Teal,
						_1: {
							ctor: '::',
							_0: _debois$elm_mdl$Material_Color$Green,
							_1: {
								ctor: '::',
								_0: _debois$elm_mdl$Material_Color$LightGreen,
								_1: {
									ctor: '::',
									_0: _debois$elm_mdl$Material_Color$Lime,
									_1: {
										ctor: '::',
										_0: _debois$elm_mdl$Material_Color$Yellow,
										_1: {
											ctor: '::',
											_0: _debois$elm_mdl$Material_Color$Amber,
											_1: {
												ctor: '::',
												_0: _debois$elm_mdl$Material_Color$Orange,
												_1: {
													ctor: '::',
													_0: _debois$elm_mdl$Material_Color$DeepOrange,
													_1: {
														ctor: '::',
														_0: _debois$elm_mdl$Material_Color$Red,
														_1: {
															ctor: '::',
															_0: _debois$elm_mdl$Material_Color$Pink,
															_1: {
																ctor: '::',
																_0: _debois$elm_mdl$Material_Color$Purple,
																_1: {
																	ctor: '::',
																	_0: _debois$elm_mdl$Material_Color$DeepPurple,
																	_1: {ctor: '[]'}
																}
															}
														}
													}
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}
		}
	});
var _debois$elm_mdl$Material_Color$A700 = {ctor: 'A700'};
var _debois$elm_mdl$Material_Color$A400 = {ctor: 'A400'};
var _debois$elm_mdl$Material_Color$A200 = {ctor: 'A200'};
var _debois$elm_mdl$Material_Color$A100 = {ctor: 'A100'};
var _debois$elm_mdl$Material_Color$S900 = {ctor: 'S900'};
var _debois$elm_mdl$Material_Color$S800 = {ctor: 'S800'};
var _debois$elm_mdl$Material_Color$S700 = {ctor: 'S700'};
var _debois$elm_mdl$Material_Color$S600 = {ctor: 'S600'};
var _debois$elm_mdl$Material_Color$S500 = {ctor: 'S500'};
var _debois$elm_mdl$Material_Color$S400 = {ctor: 'S400'};
var _debois$elm_mdl$Material_Color$S300 = {ctor: 'S300'};
var _debois$elm_mdl$Material_Color$S200 = {ctor: 'S200'};
var _debois$elm_mdl$Material_Color$S100 = {ctor: 'S100'};
var _debois$elm_mdl$Material_Color$S50 = {ctor: 'S50'};
var _debois$elm_mdl$Material_Color$shades = _elm_lang$core$Array$fromList(
	{
		ctor: '::',
		_0: _debois$elm_mdl$Material_Color$S50,
		_1: {
			ctor: '::',
			_0: _debois$elm_mdl$Material_Color$S100,
			_1: {
				ctor: '::',
				_0: _debois$elm_mdl$Material_Color$S200,
				_1: {
					ctor: '::',
					_0: _debois$elm_mdl$Material_Color$S300,
					_1: {
						ctor: '::',
						_0: _debois$elm_mdl$Material_Color$S400,
						_1: {
							ctor: '::',
							_0: _debois$elm_mdl$Material_Color$S500,
							_1: {
								ctor: '::',
								_0: _debois$elm_mdl$Material_Color$S600,
								_1: {
									ctor: '::',
									_0: _debois$elm_mdl$Material_Color$S700,
									_1: {
										ctor: '::',
										_0: _debois$elm_mdl$Material_Color$S800,
										_1: {
											ctor: '::',
											_0: _debois$elm_mdl$Material_Color$S900,
											_1: {
												ctor: '::',
												_0: _debois$elm_mdl$Material_Color$A100,
												_1: {
													ctor: '::',
													_0: _debois$elm_mdl$Material_Color$A200,
													_1: {
														ctor: '::',
														_0: _debois$elm_mdl$Material_Color$A400,
														_1: {
															ctor: '::',
															_0: _debois$elm_mdl$Material_Color$A700,
															_1: {ctor: '[]'}
														}
													}
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}
		}
	});
var _debois$elm_mdl$Material_Color$C = function (a) {
	return {ctor: 'C', _0: a};
};
var _debois$elm_mdl$Material_Color$color = F2(
	function (hue, shade) {
		return _debois$elm_mdl$Material_Color$C(
			A2(
				_elm_lang$core$Basics_ops['++'],
				_debois$elm_mdl$Material_Color$hueName(hue),
				A2(
					_elm_lang$core$Basics_ops['++'],
					'-',
					_debois$elm_mdl$Material_Color$shadeName(shade))));
	});
var _debois$elm_mdl$Material_Color$white = _debois$elm_mdl$Material_Color$C('white');
var _debois$elm_mdl$Material_Color$black = _debois$elm_mdl$Material_Color$C('black');
var _debois$elm_mdl$Material_Color$primary = _debois$elm_mdl$Material_Color$C('primary');
var _debois$elm_mdl$Material_Color$primaryDark = _debois$elm_mdl$Material_Color$C('primary-dark');
var _debois$elm_mdl$Material_Color$primaryContrast = _debois$elm_mdl$Material_Color$C('primary-contrast');
var _debois$elm_mdl$Material_Color$accent = _debois$elm_mdl$Material_Color$C('accent');
var _debois$elm_mdl$Material_Color$accentContrast = _debois$elm_mdl$Material_Color$C('accent-contrast');

var _debois$elm_mdl$Material_Footer$tempPrefix = '{{prefix}}';
var _debois$elm_mdl$Material_Footer$prefixRegex = _elm_lang$core$Regex$regex(_debois$elm_mdl$Material_Footer$tempPrefix);
var _debois$elm_mdl$Material_Footer$removePrefix = A3(
	_elm_lang$core$Regex$replace,
	_elm_lang$core$Regex$All,
	_debois$elm_mdl$Material_Footer$prefixRegex,
	function (_p0) {
		return '';
	});
var _debois$elm_mdl$Material_Footer$prefixedClass = function (cls) {
	return _debois$elm_mdl$Material_Options$cs(
		A2(_elm_lang$core$Basics_ops['++'], _debois$elm_mdl$Material_Footer$tempPrefix, cls));
};
var _debois$elm_mdl$Material_Footer$socialBtn = _debois$elm_mdl$Material_Footer$prefixedClass('social-btn');
var _debois$elm_mdl$Material_Footer$headingClass = _debois$elm_mdl$Material_Footer$prefixedClass('heading');
var _debois$elm_mdl$Material_Footer$href = function (_p1) {
	return _debois$elm_mdl$Material_Options_Internal$attribute(
		_elm_lang$html$Html_Attributes$href(_p1));
};
var _debois$elm_mdl$Material_Footer$separator = '__';
var _debois$elm_mdl$Material_Footer$prefix = function (tp) {
	var _p2 = tp;
	if (_p2.ctor === 'Mini') {
		return 'mdl-mini-footer';
	} else {
		return 'mdl-mega-footer';
	}
};
var _debois$elm_mdl$Material_Footer$applyPrefix = F2(
	function (tp, prop) {
		var sep = _debois$elm_mdl$Material_Footer$separator;
		var pref = _debois$elm_mdl$Material_Footer$prefix(tp);
		var _p3 = prop;
		switch (_p3.ctor) {
			case 'Class':
				var _p4 = _p3._0;
				return A2(_elm_lang$core$String$startsWith, _debois$elm_mdl$Material_Footer$tempPrefix, _p4) ? _debois$elm_mdl$Material_Options$cs(
					A2(
						_elm_lang$core$Basics_ops['++'],
						pref,
						A2(
							_elm_lang$core$Basics_ops['++'],
							sep,
							_debois$elm_mdl$Material_Footer$removePrefix(_p4)))) : prop;
			case 'Many':
				return _debois$elm_mdl$Material_Options$many(
					A2(
						_elm_lang$core$List$map,
						_debois$elm_mdl$Material_Footer$applyPrefix(tp),
						_p3._0));
			default:
				return prop;
		}
	});
var _debois$elm_mdl$Material_Footer$toHtml = F2(
	function (tp, _p5) {
		var _p6 = _p5;
		var styles_ = A2(
			_elm_lang$core$List$map,
			_debois$elm_mdl$Material_Footer$applyPrefix(tp),
			_p6.styles);
		return A3(
			_debois$elm_mdl$Material_Options$styled,
			_p6.elem,
			styles_,
			A2(
				_elm_lang$core$List$map,
				_debois$elm_mdl$Material_Footer$contentToHtml(tp),
				_p6.content));
	});
var _debois$elm_mdl$Material_Footer$contentToHtml = F2(
	function (tp, content) {
		var _p7 = content;
		if (_p7.ctor === 'HtmlContent') {
			return _p7._0;
		} else {
			return A2(_debois$elm_mdl$Material_Footer$toHtml, tp, _p7._0);
		}
	});
var _debois$elm_mdl$Material_Footer$sectionContent = F3(
	function (tp, section, content) {
		var sep = _debois$elm_mdl$Material_Footer$separator;
		var pref = _debois$elm_mdl$Material_Footer$prefix(tp);
		var _p8 = content;
		if (_p8.ctor === 'HtmlContent') {
			return A3(
				_debois$elm_mdl$Material_Options$styled,
				_elm_lang$html$Html$div,
				{
					ctor: '::',
					_0: _debois$elm_mdl$Material_Options$cs(
						A2(
							_elm_lang$core$Basics_ops['++'],
							pref,
							A2(_elm_lang$core$Basics_ops['++'], sep, section))),
					_1: {ctor: '[]'}
				},
				{
					ctor: '::',
					_0: _p8._0,
					_1: {ctor: '[]'}
				});
		} else {
			return A3(
				_debois$elm_mdl$Material_Options$styled,
				_p8._0.elem,
				{
					ctor: '::',
					_0: _debois$elm_mdl$Material_Options$cs(
						A2(
							_elm_lang$core$Basics_ops['++'],
							pref,
							A2(_elm_lang$core$Basics_ops['++'], sep, section))),
					_1: _p8._0.styles
				},
				A2(
					_elm_lang$core$List$map,
					_debois$elm_mdl$Material_Footer$contentToHtml(tp),
					_p8._0.content));
		}
	});
var _debois$elm_mdl$Material_Footer$leftHtml = F2(
	function (tp, left) {
		var _p9 = left;
		if (_p9.ctor === 'Just') {
			return {
				ctor: '::',
				_0: A3(_debois$elm_mdl$Material_Footer$sectionContent, tp, 'left-section', _p9._0._0),
				_1: {ctor: '[]'}
			};
		} else {
			return {ctor: '[]'};
		}
	});
var _debois$elm_mdl$Material_Footer$rightHtml = F2(
	function (tp, right) {
		var _p10 = right;
		if (_p10.ctor === 'Just') {
			return {
				ctor: '::',
				_0: A3(_debois$elm_mdl$Material_Footer$sectionContent, tp, 'right-section', _p10._0._0),
				_1: {ctor: '[]'}
			};
		} else {
			return {ctor: '[]'};
		}
	});
var _debois$elm_mdl$Material_Footer$Footer = F3(
	function (a, b, c) {
		return {styles: a, content: b, elem: c};
	});
var _debois$elm_mdl$Material_Footer$MiniFooter = F2(
	function (a, b) {
		return {left: a, right: b};
	});
var _debois$elm_mdl$Material_Footer$MegaFooter = F3(
	function (a, b, c) {
		return {top: a, bottom: b, middle: c};
	});
var _debois$elm_mdl$Material_Footer$Mega = {ctor: 'Mega'};
var _debois$elm_mdl$Material_Footer$mega = F2(
	function (props, _p11) {
		var _p12 = _p11;
		var sep = _debois$elm_mdl$Material_Footer$separator;
		var tp = _debois$elm_mdl$Material_Footer$Mega;
		var pref = _debois$elm_mdl$Material_Footer$prefix(tp);
		var topContent = function () {
			var _p13 = _p12.top;
			if (_p13.ctor === 'Nothing') {
				return {ctor: '[]'};
			} else {
				return {
					ctor: '::',
					_0: A3(
						_debois$elm_mdl$Material_Options$styled,
						_elm_lang$html$Html$div,
						{
							ctor: '::',
							_0: _debois$elm_mdl$Material_Options$cs(
								A2(
									_elm_lang$core$Basics_ops['++'],
									pref,
									A2(_elm_lang$core$Basics_ops['++'], sep, 'top-section'))),
							_1: _p13._0._0.props
						},
						A2(
							_elm_lang$core$Basics_ops['++'],
							A2(_debois$elm_mdl$Material_Footer$leftHtml, tp, _p13._0._0.left),
							A2(_debois$elm_mdl$Material_Footer$rightHtml, tp, _p13._0._0.right))),
					_1: {ctor: '[]'}
				};
			}
		}();
		var middleContent = function () {
			var _p14 = _p12.middle;
			if (_p14.ctor === 'Nothing') {
				return {ctor: '[]'};
			} else {
				return {
					ctor: '::',
					_0: A3(
						_debois$elm_mdl$Material_Options$styled,
						_elm_lang$html$Html$div,
						{
							ctor: '::',
							_0: _debois$elm_mdl$Material_Options$cs(
								A2(
									_elm_lang$core$Basics_ops['++'],
									pref,
									A2(_elm_lang$core$Basics_ops['++'], sep, 'middle-section'))),
							_1: _p14._0._0.props
						},
						A2(
							_elm_lang$core$List$map,
							_debois$elm_mdl$Material_Footer$contentToHtml(tp),
							_p14._0._0.content)),
					_1: {ctor: '[]'}
				};
			}
		}();
		var bottomContent = function () {
			var _p15 = _p12.bottom;
			if (_p15.ctor === 'Nothing') {
				return {ctor: '[]'};
			} else {
				return {
					ctor: '::',
					_0: A3(
						_debois$elm_mdl$Material_Options$styled,
						_elm_lang$html$Html$div,
						{
							ctor: '::',
							_0: _debois$elm_mdl$Material_Options$cs(
								A2(
									_elm_lang$core$Basics_ops['++'],
									pref,
									A2(_elm_lang$core$Basics_ops['++'], sep, 'bottom-section'))),
							_1: _p15._0._0.props
						},
						A2(
							_elm_lang$core$Basics_ops['++'],
							{ctor: '[]'},
							A2(
								_elm_lang$core$List$map,
								_debois$elm_mdl$Material_Footer$contentToHtml(tp),
								_p15._0._0.content))),
					_1: {ctor: '[]'}
				};
			}
		}();
		return A3(
			_debois$elm_mdl$Material_Options$styled,
			_elm_lang$html$Html$footer,
			{
				ctor: '::',
				_0: _debois$elm_mdl$Material_Options$cs(pref),
				_1: props
			},
			A2(
				_elm_lang$core$Basics_ops['++'],
				topContent,
				A2(_elm_lang$core$Basics_ops['++'], middleContent, bottomContent)));
	});
var _debois$elm_mdl$Material_Footer$Mini = {ctor: 'Mini'};
var _debois$elm_mdl$Material_Footer$mini = F2(
	function (props, _p16) {
		var _p17 = _p16;
		var tp = _debois$elm_mdl$Material_Footer$Mini;
		var pref = _debois$elm_mdl$Material_Footer$prefix(tp);
		var leftContent = A2(_debois$elm_mdl$Material_Footer$leftHtml, tp, _p17.left);
		var rightContent = A2(_debois$elm_mdl$Material_Footer$rightHtml, tp, _p17.right);
		return A3(
			_debois$elm_mdl$Material_Options$styled,
			_elm_lang$html$Html$footer,
			{
				ctor: '::',
				_0: _debois$elm_mdl$Material_Options$cs(pref),
				_1: props
			},
			A2(_elm_lang$core$Basics_ops['++'], leftContent, rightContent));
	});
var _debois$elm_mdl$Material_Footer$FooterProperty = {ctor: 'FooterProperty'};
var _debois$elm_mdl$Material_Footer$Content = function (a) {
	return {ctor: 'Content', _0: a};
};
var _debois$elm_mdl$Material_Footer$logo = F2(
	function (styles, content) {
		return _debois$elm_mdl$Material_Footer$Content(
			{
				styles: {
					ctor: '::',
					_0: _debois$elm_mdl$Material_Options$cs('mdl-logo'),
					_1: styles
				},
				content: content,
				elem: _elm_lang$html$Html$div
			});
	});
var _debois$elm_mdl$Material_Footer$links = F2(
	function (styles, content) {
		return _debois$elm_mdl$Material_Footer$Content(
			{
				styles: {
					ctor: '::',
					_0: _debois$elm_mdl$Material_Footer$prefixedClass('link-list'),
					_1: styles
				},
				content: content,
				elem: _elm_lang$html$Html$ul
			});
	});
var _debois$elm_mdl$Material_Footer$linkItem = F2(
	function (styles, content) {
		return _debois$elm_mdl$Material_Footer$Content(
			{
				styles: {ctor: '[]'},
				content: {
					ctor: '::',
					_0: _debois$elm_mdl$Material_Footer$Content(
						{styles: styles, content: content, elem: _elm_lang$html$Html$a}),
					_1: {ctor: '[]'}
				},
				elem: _elm_lang$html$Html$li
			});
	});
var _debois$elm_mdl$Material_Footer$heading = F2(
	function (styles, content) {
		return _debois$elm_mdl$Material_Footer$Content(
			{
				styles: {ctor: '::', _0: _debois$elm_mdl$Material_Footer$headingClass, _1: styles},
				content: content,
				elem: _elm_lang$html$Html$h1
			});
	});
var _debois$elm_mdl$Material_Footer$socialButton = F2(
	function (styles, content) {
		return _debois$elm_mdl$Material_Footer$Content(
			{
				styles: {ctor: '::', _0: _debois$elm_mdl$Material_Footer$socialBtn, _1: styles},
				content: content,
				elem: _elm_lang$html$Html$button
			});
	});
var _debois$elm_mdl$Material_Footer$HtmlContent = function (a) {
	return {ctor: 'HtmlContent', _0: a};
};
var _debois$elm_mdl$Material_Footer$checkbox = _debois$elm_mdl$Material_Footer$HtmlContent(
	A2(
		_elm_lang$html$Html$input,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$class('mdl-mega-footer__heading-checkbox'),
			_1: {
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$type_('checkbox'),
				_1: {
					ctor: '::',
					_0: A2(_elm_lang$html$Html_Attributes$attribute, 'checked', ''),
					_1: {ctor: '[]'}
				}
			}
		},
		{ctor: '[]'}));
var _debois$elm_mdl$Material_Footer$dropdown = F2(
	function (props, content) {
		return _debois$elm_mdl$Material_Footer$Content(
			{
				styles: {
					ctor: '::',
					_0: _debois$elm_mdl$Material_Options$cs('mdl-mega-footer__drop-down-section'),
					_1: props
				},
				content: {ctor: '::', _0: _debois$elm_mdl$Material_Footer$checkbox, _1: content},
				elem: _elm_lang$html$Html$div
			});
	});
var _debois$elm_mdl$Material_Footer$html = _debois$elm_mdl$Material_Footer$HtmlContent;
var _debois$elm_mdl$Material_Footer$link = F2(
	function (styles, contents) {
		return _debois$elm_mdl$Material_Footer$html(
			A3(_debois$elm_mdl$Material_Options$styled, _elm_lang$html$Html$a, styles, contents));
	});
var _debois$elm_mdl$Material_Footer$li = F2(
	function (styles, content) {
		return _debois$elm_mdl$Material_Footer$html(
			A3(_debois$elm_mdl$Material_Options$styled, _elm_lang$html$Html$li, styles, content));
	});
var _debois$elm_mdl$Material_Footer$Section = function (a) {
	return {ctor: 'Section', _0: a};
};
var _debois$elm_mdl$Material_Footer$left = F2(
	function (styles, content) {
		return function (_p18) {
			return _elm_lang$core$Maybe$Just(
				_debois$elm_mdl$Material_Footer$Section(_p18));
		}(
			_debois$elm_mdl$Material_Footer$Content(
				{styles: styles, content: content, elem: _elm_lang$html$Html$div}));
	});
var _debois$elm_mdl$Material_Footer$right = F2(
	function (styles, content) {
		return function (_p19) {
			return _elm_lang$core$Maybe$Just(
				_debois$elm_mdl$Material_Footer$Section(_p19));
		}(
			_debois$elm_mdl$Material_Footer$Content(
				{styles: styles, content: content, elem: _elm_lang$html$Html$div}));
	});
var _debois$elm_mdl$Material_Footer$TopSection = function (a) {
	return {ctor: 'TopSection', _0: a};
};
var _debois$elm_mdl$Material_Footer$top = F2(
	function (props, _p20) {
		var _p21 = _p20;
		return _elm_lang$core$Maybe$Just(
			_debois$elm_mdl$Material_Footer$TopSection(
				{left: _p21.left, right: _p21.right, props: props}));
	});
var _debois$elm_mdl$Material_Footer$BottomSection = function (a) {
	return {ctor: 'BottomSection', _0: a};
};
var _debois$elm_mdl$Material_Footer$bottom = F2(
	function (props, content) {
		return _elm_lang$core$Maybe$Just(
			_debois$elm_mdl$Material_Footer$BottomSection(
				{props: props, content: content}));
	});
var _debois$elm_mdl$Material_Footer$MiddleSection = function (a) {
	return {ctor: 'MiddleSection', _0: a};
};
var _debois$elm_mdl$Material_Footer$middle = F2(
	function (props, content) {
		return _elm_lang$core$Maybe$Just(
			_debois$elm_mdl$Material_Footer$MiddleSection(
				{props: props, content: content}));
	});

var _debois$elm_mdl$Material_Grid$clip = F3(
	function (lower, upper, k) {
		return A2(
			_elm_lang$core$Basics$max,
			lower,
			A2(_elm_lang$core$Basics$min, k, upper));
	});
var _debois$elm_mdl$Material_Grid$stretch = _debois$elm_mdl$Material_Options$cs('mdl-cell--stretch');
var _debois$elm_mdl$Material_Grid$align = function (a) {
	var _p0 = a;
	switch (_p0.ctor) {
		case 'Top':
			return _debois$elm_mdl$Material_Options$cs('mdl-cell--top');
		case 'Middle':
			return _debois$elm_mdl$Material_Options$cs('mdl-cell--middle');
		default:
			return _debois$elm_mdl$Material_Options$cs('mdl-cell--bottom');
	}
};
var _debois$elm_mdl$Material_Grid$suffix = function (device) {
	var _p1 = device;
	switch (_p1.ctor) {
		case 'All':
			return '';
		case 'Desktop':
			return '-desktop';
		case 'Tablet':
			return '-tablet';
		default:
			return '-phone';
	}
};
var _debois$elm_mdl$Material_Grid$size = F2(
	function (device, k) {
		var c = function () {
			var _p2 = device;
			switch (_p2.ctor) {
				case 'All':
					return A3(_debois$elm_mdl$Material_Grid$clip, 1, 12, k);
				case 'Desktop':
					return A3(_debois$elm_mdl$Material_Grid$clip, 1, 12, k);
				case 'Tablet':
					return A3(_debois$elm_mdl$Material_Grid$clip, 1, 8, k);
				default:
					return A3(_debois$elm_mdl$Material_Grid$clip, 1, 4, k);
			}
		}();
		return _debois$elm_mdl$Material_Options$cs(
			A2(
				_elm_lang$core$Basics_ops['++'],
				'mdl-cell--',
				A2(
					_elm_lang$core$Basics_ops['++'],
					_elm_lang$core$Basics$toString(c),
					A2(
						_elm_lang$core$Basics_ops['++'],
						'-col',
						_debois$elm_mdl$Material_Grid$suffix(device)))));
	});
var _debois$elm_mdl$Material_Grid$offset = F2(
	function (device, k) {
		var c = function () {
			var _p3 = device;
			switch (_p3.ctor) {
				case 'All':
					return A3(_debois$elm_mdl$Material_Grid$clip, 0, 11, k);
				case 'Desktop':
					return A3(_debois$elm_mdl$Material_Grid$clip, 0, 11, k);
				case 'Tablet':
					return A3(_debois$elm_mdl$Material_Grid$clip, 0, 7, k);
				default:
					return A3(_debois$elm_mdl$Material_Grid$clip, 0, 3, k);
			}
		}();
		return _debois$elm_mdl$Material_Options$cs(
			A2(
				_elm_lang$core$Basics_ops['++'],
				'mdl-cell--',
				A2(
					_elm_lang$core$Basics_ops['++'],
					_elm_lang$core$Basics$toString(c),
					A2(
						_elm_lang$core$Basics_ops['++'],
						'-offset',
						_debois$elm_mdl$Material_Grid$suffix(device)))));
	});
var _debois$elm_mdl$Material_Grid$hide = function (device) {
	return _debois$elm_mdl$Material_Options$cs(
		function () {
			var _p4 = device;
			if (_p4.ctor === 'All') {
				return '';
			} else {
				return A2(
					_elm_lang$core$Basics_ops['++'],
					'mdl-cell--hide-',
					_debois$elm_mdl$Material_Grid$suffix(device));
			}
		}());
};
var _debois$elm_mdl$Material_Grid$order = F2(
	function (device, n) {
		return _debois$elm_mdl$Material_Options$cs(
			A2(
				_elm_lang$core$Basics_ops['++'],
				'mdl-cell--order-',
				A2(
					_elm_lang$core$Basics_ops['++'],
					_elm_lang$core$Basics$toString(
						A3(_debois$elm_mdl$Material_Grid$clip, 1, 12, n)),
					_debois$elm_mdl$Material_Grid$suffix(device))));
	});
var _debois$elm_mdl$Material_Grid$grid = F2(
	function (styling, cells) {
		return A2(
			_debois$elm_mdl$Material_Options$div,
			{
				ctor: '::',
				_0: _debois$elm_mdl$Material_Options$cs('mdl-grid'),
				_1: styling
			},
			A2(
				_elm_lang$core$List$map,
				function (_p5) {
					var _p6 = _p5;
					return _p6._0;
				},
				cells));
	});
var _debois$elm_mdl$Material_Grid$maxWidth = function (w) {
	return A2(_debois$elm_mdl$Material_Options$css, 'max-width', w);
};
var _debois$elm_mdl$Material_Grid$noSpacing = _debois$elm_mdl$Material_Options$cs('mdl-grid--no-spacing');
var _debois$elm_mdl$Material_Grid$Phone = {ctor: 'Phone'};
var _debois$elm_mdl$Material_Grid$Tablet = {ctor: 'Tablet'};
var _debois$elm_mdl$Material_Grid$Desktop = {ctor: 'Desktop'};
var _debois$elm_mdl$Material_Grid$All = {ctor: 'All'};
var _debois$elm_mdl$Material_Grid$Cell = function (a) {
	return {ctor: 'Cell', _0: a};
};
var _debois$elm_mdl$Material_Grid$cell = F2(
	function (styling, elms) {
		return _debois$elm_mdl$Material_Grid$Cell(
			A2(
				_debois$elm_mdl$Material_Options$div,
				{
					ctor: '::',
					_0: _debois$elm_mdl$Material_Options$cs('mdl-cell'),
					_1: styling
				},
				elms));
	});
var _debois$elm_mdl$Material_Grid$Bottom = {ctor: 'Bottom'};
var _debois$elm_mdl$Material_Grid$Middle = {ctor: 'Middle'};
var _debois$elm_mdl$Material_Grid$Top = {ctor: 'Top'};

var _debois$elm_mdl$Material_List$action2 = _debois$elm_mdl$Material_Options$cs('mdl-list__item-secondary-action');
var _debois$elm_mdl$Material_List$info2 = function (options) {
	return _debois$elm_mdl$Material_Options$span(
		{
			ctor: '::',
			_0: _debois$elm_mdl$Material_Options$cs('mdl-list__item-secondary-info'),
			_1: options
		});
};
var _debois$elm_mdl$Material_List$content2 = function (options) {
	return _debois$elm_mdl$Material_Options$span(
		{
			ctor: '::',
			_0: _debois$elm_mdl$Material_Options$cs('mdl-list__item-secondary-content'),
			_1: options
		});
};
var _debois$elm_mdl$Material_List$subtitle = function (options) {
	return _debois$elm_mdl$Material_Options$span(
		{
			ctor: '::',
			_0: _debois$elm_mdl$Material_Options$cs('mdl-list__item-sub-title'),
			_1: options
		});
};
var _debois$elm_mdl$Material_List$body = function (options) {
	return _debois$elm_mdl$Material_Options$span(
		{
			ctor: '::',
			_0: _debois$elm_mdl$Material_Options$cs('mdl-list__item-text-body'),
			_1: options
		});
};
var _debois$elm_mdl$Material_List$icon = F2(
	function (i, options) {
		return A2(
			_debois$elm_mdl$Material_Icon$view,
			i,
			{
				ctor: '::',
				_0: _debois$elm_mdl$Material_Options$cs('mdl-list__item-icon'),
				_1: options
			});
	});
var _debois$elm_mdl$Material_List$avatar = _debois$elm_mdl$Material_Options$cs('mdl-list__item-avatar');
var _debois$elm_mdl$Material_List$avatarImage = F2(
	function (src, options) {
		return A4(
			_debois$elm_mdl$Material_Options$styled_,
			_elm_lang$html$Html$img,
			{ctor: '::', _0: _debois$elm_mdl$Material_List$avatar, _1: options},
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$src(src),
				_1: {ctor: '[]'}
			},
			{ctor: '[]'});
	});
var _debois$elm_mdl$Material_List$avatarIcon = F2(
	function (i, options) {
		return A2(
			_debois$elm_mdl$Material_Options$div,
			{
				ctor: '::',
				_0: _debois$elm_mdl$Material_Options$center,
				_1: {
					ctor: '::',
					_0: _debois$elm_mdl$Material_Options$many(options),
					_1: {
						ctor: '::',
						_0: _debois$elm_mdl$Material_List$avatar,
						_1: {ctor: '[]'}
					}
				}
			},
			{
				ctor: '::',
				_0: _debois$elm_mdl$Material_Icon$i(i),
				_1: {ctor: '[]'}
			});
	});
var _debois$elm_mdl$Material_List$content = function (options) {
	return _debois$elm_mdl$Material_Options$span(
		{
			ctor: '::',
			_0: _debois$elm_mdl$Material_Options$cs('mdl-list__item-primary-content'),
			_1: options
		});
};
var _debois$elm_mdl$Material_List$withSubtitle = _debois$elm_mdl$Material_Options$cs('mdl-list__item--two-line');
var _debois$elm_mdl$Material_List$withBody = _debois$elm_mdl$Material_Options$cs('mdl-list__item--three-line');
var _debois$elm_mdl$Material_List$li = function (options) {
	return A2(
		_debois$elm_mdl$Material_Options$styled,
		_elm_lang$html$Html$li,
		{
			ctor: '::',
			_0: _debois$elm_mdl$Material_Options$cs('mdl-list__item'),
			_1: options
		});
};
var _debois$elm_mdl$Material_List$ul = function (options) {
	return A2(
		_debois$elm_mdl$Material_Options$styled,
		_elm_lang$html$Html$ul,
		{
			ctor: '::',
			_0: _debois$elm_mdl$Material_Options$cs('mdl-list'),
			_1: options
		});
};

var _debois$elm_mdl$Material_Slider$floatVal = A2(
	_elm_lang$core$Json_Decode$at,
	{
		ctor: '::',
		_0: 'target',
		_1: {
			ctor: '::',
			_0: 'valueAsNumber',
			_1: {ctor: '[]'}
		}
	},
	_elm_lang$core$Json_Decode$float);
var _debois$elm_mdl$Material_Slider$onChange = function (f) {
	return _debois$elm_mdl$Material_Options$many(
		{
			ctor: '::',
			_0: A2(
				_debois$elm_mdl$Material_Options$on,
				'change',
				A2(_elm_lang$core$Json_Decode$map, f, _debois$elm_mdl$Material_Slider$floatVal)),
			_1: {
				ctor: '::',
				_0: A2(
					_debois$elm_mdl$Material_Options$on,
					'input',
					A2(_elm_lang$core$Json_Decode$map, f, _debois$elm_mdl$Material_Slider$floatVal)),
				_1: {ctor: '[]'}
			}
		});
};
var _debois$elm_mdl$Material_Slider$disabled = _debois$elm_mdl$Material_Options_Internal$attribute(
	_elm_lang$html$Html_Attributes$disabled(true));
var _debois$elm_mdl$Material_Slider$step = function (_p0) {
	return _debois$elm_mdl$Material_Options_Internal$option(
		F2(
			function (v, options) {
				return _elm_lang$core$Native_Utils.update(
					options,
					{step: v});
			})(_p0));
};
var _debois$elm_mdl$Material_Slider$max = function (_p1) {
	return _debois$elm_mdl$Material_Options_Internal$option(
		F2(
			function (v, options) {
				return _elm_lang$core$Native_Utils.update(
					options,
					{max: v});
			})(_p1));
};
var _debois$elm_mdl$Material_Slider$min = function (_p2) {
	return _debois$elm_mdl$Material_Options_Internal$option(
		F2(
			function (v, options) {
				return _elm_lang$core$Native_Utils.update(
					options,
					{min: v});
			})(_p2));
};
var _debois$elm_mdl$Material_Slider$value = function (_p3) {
	return _debois$elm_mdl$Material_Options_Internal$option(
		F2(
			function (v, options) {
				return _elm_lang$core$Native_Utils.update(
					options,
					{value: v});
			})(_p3));
};
var _debois$elm_mdl$Material_Slider$defaultConfig = {
	value: 50,
	min: 0,
	max: 100,
	step: 1,
	input: {ctor: '[]'},
	container: {ctor: '[]'}
};
var _debois$elm_mdl$Material_Slider$view = function (options) {
	var summary = A2(_debois$elm_mdl$Material_Options_Internal$collect, _debois$elm_mdl$Material_Slider$defaultConfig, options);
	var config = summary.config;
	var fraction = (config.value - config.min) / (config.max - config.min);
	var lower = A2(
		_elm_lang$core$Basics_ops['++'],
		_elm_lang$core$Basics$toString(fraction),
		' 1 0%');
	var upper = A2(
		_elm_lang$core$Basics_ops['++'],
		_elm_lang$core$Basics$toString(1 - fraction),
		' 1 0%');
	var background = A3(
		_debois$elm_mdl$Material_Options$styled,
		_elm_lang$html$Html$div,
		{
			ctor: '::',
			_0: _debois$elm_mdl$Material_Options$cs('mdl-slider__background-flex'),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: A3(
				_debois$elm_mdl$Material_Options$styled,
				_elm_lang$html$Html$div,
				{
					ctor: '::',
					_0: _debois$elm_mdl$Material_Options$cs('mdl-slider__background-lower'),
					_1: {
						ctor: '::',
						_0: A2(_debois$elm_mdl$Material_Options$css, 'flex', lower),
						_1: {ctor: '[]'}
					}
				},
				{ctor: '[]'}),
			_1: {
				ctor: '::',
				_0: A3(
					_debois$elm_mdl$Material_Options$styled,
					_elm_lang$html$Html$div,
					{
						ctor: '::',
						_0: _debois$elm_mdl$Material_Options$cs('mdl-slider__background-upper'),
						_1: {
							ctor: '::',
							_0: A2(_debois$elm_mdl$Material_Options$css, 'flex', upper),
							_1: {ctor: '[]'}
						}
					},
					{ctor: '[]'}),
				_1: {ctor: '[]'}
			}
		});
	return A4(
		_debois$elm_mdl$Material_Options_Internal$applyContainer,
		summary,
		_elm_lang$html$Html$div,
		{
			ctor: '::',
			_0: _debois$elm_mdl$Material_Options$cs('mdl-slider__container'),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: A4(
				_debois$elm_mdl$Material_Options_Internal$applyInput,
				summary,
				_elm_lang$html$Html$input,
				{
					ctor: '::',
					_0: _debois$elm_mdl$Material_Options$cs('mdl-slider'),
					_1: {
						ctor: '::',
						_0: _debois$elm_mdl$Material_Options$cs('mdl-js-slider'),
						_1: {
							ctor: '::',
							_0: _debois$elm_mdl$Material_Options$cs('is-upgraded'),
							_1: {
								ctor: '::',
								_0: A2(
									_debois$elm_mdl$Material_Options$when,
									_elm_lang$core$Native_Utils.eq(fraction, 0),
									_debois$elm_mdl$Material_Options$cs('is-lowest-value')),
								_1: {
									ctor: '::',
									_0: A2(_debois$elm_mdl$Material_Options$css, 'padding', '8px 0'),
									_1: {
										ctor: '::',
										_0: _debois$elm_mdl$Material_Options_Internal$attribute(
											_elm_lang$html$Html_Attributes$type_('range')),
										_1: {
											ctor: '::',
											_0: _debois$elm_mdl$Material_Options_Internal$attribute(
												_elm_lang$html$Html_Attributes$max(
													_elm_lang$core$Basics$toString(config.max))),
											_1: {
												ctor: '::',
												_0: _debois$elm_mdl$Material_Options_Internal$attribute(
													_elm_lang$html$Html_Attributes$min(
														_elm_lang$core$Basics$toString(config.min))),
												_1: {
													ctor: '::',
													_0: _debois$elm_mdl$Material_Options_Internal$attribute(
														_elm_lang$html$Html_Attributes$step(
															_elm_lang$core$Basics$toString(config.step))),
													_1: {
														ctor: '::',
														_0: _debois$elm_mdl$Material_Options_Internal$attribute(
															_elm_lang$html$Html_Attributes$value(
																_elm_lang$core$Basics$toString(config.value))),
														_1: {
															ctor: '::',
															_0: _debois$elm_mdl$Material_Options_Internal$attribute(
																_debois$elm_mdl$Material_Helpers$blurOn('mouseup')),
															_1: {ctor: '[]'}
														}
													}
												}
											}
										}
									}
								}
							}
						}
					}
				},
				{ctor: '[]'}),
			_1: {
				ctor: '::',
				_0: background,
				_1: {ctor: '[]'}
			}
		});
};
var _debois$elm_mdl$Material_Slider$Config = F6(
	function (a, b, c, d, e, f) {
		return {value: a, min: b, max: c, step: d, input: e, container: f};
	});

var _debois$elm_mdl$Material_Table$defaultCell = {numeric: false};
var _debois$elm_mdl$Material_Table$td = F2(
	function (options, html) {
		var _p0 = A2(_debois$elm_mdl$Material_Options_Internal$collect, _debois$elm_mdl$Material_Table$defaultCell, options);
		var summary = _p0;
		var config = _p0.config;
		return A5(
			_debois$elm_mdl$Material_Options_Internal$apply,
			summary,
			_elm_lang$html$Html$td,
			{
				ctor: '::',
				_0: A2(
					_debois$elm_mdl$Material_Options$when,
					config.numeric,
					_debois$elm_mdl$Material_Options$cs('mdl-data-table__cell--non-numeric')),
				_1: {ctor: '[]'}
			},
			{ctor: '[]'},
			html);
	});
var _debois$elm_mdl$Material_Table$sorted = function (order) {
	return _debois$elm_mdl$Material_Options_Internal$option(
		function (self) {
			return _elm_lang$core$Native_Utils.update(
				self,
				{
					sorted: _elm_lang$core$Maybe$Just(order)
				});
		});
};
var _debois$elm_mdl$Material_Table$numeric = _debois$elm_mdl$Material_Options_Internal$option(
	function (self) {
		return _elm_lang$core$Native_Utils.update(
			self,
			{numeric: true});
	});
var _debois$elm_mdl$Material_Table$defaultHeader = {numeric: false, sorted: _elm_lang$core$Maybe$Nothing};
var _debois$elm_mdl$Material_Table$th = F2(
	function (options, html) {
		var _p1 = A2(_debois$elm_mdl$Material_Options_Internal$collect, _debois$elm_mdl$Material_Table$defaultHeader, options);
		var summary = _p1;
		var config = _p1.config;
		return A5(
			_debois$elm_mdl$Material_Options_Internal$apply,
			summary,
			_elm_lang$html$Html$th,
			{
				ctor: '::',
				_0: A2(
					_debois$elm_mdl$Material_Options$when,
					config.numeric,
					_debois$elm_mdl$Material_Options$cs('mdl-data-table__cell--non-numeric')),
				_1: {
					ctor: '::',
					_0: function () {
						var _p2 = config.sorted;
						if (_p2.ctor === 'Just') {
							if (_p2._0.ctor === 'Ascending') {
								return _debois$elm_mdl$Material_Options$cs('mdl-data-table__header--sorted-ascending');
							} else {
								return _debois$elm_mdl$Material_Options$cs('mdl-data-table__header--sorted-descending');
							}
						} else {
							return _debois$elm_mdl$Material_Options$nop;
						}
					}(),
					_1: {ctor: '[]'}
				}
			},
			{ctor: '[]'},
			html);
	});
var _debois$elm_mdl$Material_Table$selected = _debois$elm_mdl$Material_Options_Internal$option(
	function (self) {
		return _elm_lang$core$Native_Utils.update(
			self,
			{selected: true});
	});
var _debois$elm_mdl$Material_Table$defaultRow = {selected: false};
var _debois$elm_mdl$Material_Table$tr = F2(
	function (options, html) {
		var _p3 = A2(_debois$elm_mdl$Material_Options_Internal$collect, _debois$elm_mdl$Material_Table$defaultRow, options);
		var summary = _p3;
		var config = _p3.config;
		return A5(
			_debois$elm_mdl$Material_Options_Internal$apply,
			summary,
			_elm_lang$html$Html$tr,
			{
				ctor: '::',
				_0: A2(
					_debois$elm_mdl$Material_Options$when,
					config.selected,
					_debois$elm_mdl$Material_Options$cs('is-selected')),
				_1: {ctor: '[]'}
			},
			{ctor: '[]'},
			html);
	});
var _debois$elm_mdl$Material_Table$tfoot = F2(
	function (options, html) {
		var summary = A2(
			_debois$elm_mdl$Material_Options_Internal$collect,
			{},
			options);
		return A5(
			_debois$elm_mdl$Material_Options_Internal$apply,
			summary,
			_elm_lang$html$Html$tfoot,
			{ctor: '[]'},
			{ctor: '[]'},
			html);
	});
var _debois$elm_mdl$Material_Table$tbody = F2(
	function (options, html) {
		var summary = A2(
			_debois$elm_mdl$Material_Options_Internal$collect,
			{},
			options);
		return A5(
			_debois$elm_mdl$Material_Options_Internal$apply,
			summary,
			_elm_lang$html$Html$tbody,
			{ctor: '[]'},
			{ctor: '[]'},
			html);
	});
var _debois$elm_mdl$Material_Table$thead = F2(
	function (options, html) {
		var summary = A2(
			_debois$elm_mdl$Material_Options_Internal$collect,
			{},
			options);
		return A5(
			_debois$elm_mdl$Material_Options_Internal$apply,
			summary,
			_elm_lang$html$Html$thead,
			{ctor: '[]'},
			{ctor: '[]'},
			html);
	});
var _debois$elm_mdl$Material_Table$table = F2(
	function (options, nodes) {
		return A3(
			_debois$elm_mdl$Material_Options$styled,
			_elm_lang$html$Html$table,
			{
				ctor: '::',
				_0: _debois$elm_mdl$Material_Options$cs('mdl-data-table'),
				_1: {
					ctor: '::',
					_0: _debois$elm_mdl$Material_Options$cs('mdl-js-data-table'),
					_1: {
						ctor: '::',
						_0: _debois$elm_mdl$Material_Options$cs('is-upgraded'),
						_1: options
					}
				}
			},
			nodes);
	});
var _debois$elm_mdl$Material_Table$Row = function (a) {
	return {selected: a};
};
var _debois$elm_mdl$Material_Table$Header = F2(
	function (a, b) {
		return {numeric: a, sorted: b};
	});
var _debois$elm_mdl$Material_Table$Cell = function (a) {
	return {numeric: a};
};
var _debois$elm_mdl$Material_Table$Descending = {ctor: 'Descending'};
var _debois$elm_mdl$Material_Table$descending = _debois$elm_mdl$Material_Table$sorted(_debois$elm_mdl$Material_Table$Descending);
var _debois$elm_mdl$Material_Table$Ascending = {ctor: 'Ascending'};
var _debois$elm_mdl$Material_Table$ascending = _debois$elm_mdl$Material_Table$sorted(_debois$elm_mdl$Material_Table$Ascending);

var _debois$elm_mdl$Material_Typography$uppercase = _debois$elm_mdl$Material_Options$cs('mdl-typography--text-uppercase');
var _debois$elm_mdl$Material_Typography$lowercase = _debois$elm_mdl$Material_Options$cs('mdl-typography--text-lowercase');
var _debois$elm_mdl$Material_Typography$capitalize = _debois$elm_mdl$Material_Options$cs('mdl-typography--text-capitalize');
var _debois$elm_mdl$Material_Typography$justify = _debois$elm_mdl$Material_Options$cs('mdl-typography--text-justify');
var _debois$elm_mdl$Material_Typography$right = _debois$elm_mdl$Material_Options$cs('mdl-typography--text-right');
var _debois$elm_mdl$Material_Typography$left = _debois$elm_mdl$Material_Options$cs('mdl-typography--text-left');
var _debois$elm_mdl$Material_Typography$center = _debois$elm_mdl$Material_Options$cs('mdl-typography--text-center');
var _debois$elm_mdl$Material_Typography$tableStriped = _debois$elm_mdl$Material_Options$cs('mdl-typography--table-striped');
var _debois$elm_mdl$Material_Typography$nowrap = _debois$elm_mdl$Material_Options$cs('mdl-typography--text-nowrap');
var _debois$elm_mdl$Material_Typography$contrast = function (x) {
	return A2(
		_debois$elm_mdl$Material_Options$css,
		'opacity',
		_elm_lang$core$Basics$toString(x));
};
var _debois$elm_mdl$Material_Typography$menu = _debois$elm_mdl$Material_Options$cs('mdl-typography--menu-color-contrast');
var _debois$elm_mdl$Material_Typography$button = _debois$elm_mdl$Material_Options$cs('mdl-typography--button-color-contrast');
var _debois$elm_mdl$Material_Typography$caption = _debois$elm_mdl$Material_Options$cs('mdl-typography--caption-force-preferred-font-color-contrast');
var _debois$elm_mdl$Material_Typography$body2 = _debois$elm_mdl$Material_Options$cs('mdl-typography--body-2-force-preferred-font-color-contrast');
var _debois$elm_mdl$Material_Typography$body1 = _debois$elm_mdl$Material_Options$cs('mdl-typography--body-1-force-preferred-font-color-contrast');
var _debois$elm_mdl$Material_Typography$subhead = _debois$elm_mdl$Material_Options$cs('mdl-typography--subhead-color-contrast');
var _debois$elm_mdl$Material_Typography$title = _debois$elm_mdl$Material_Options$cs('mdl-typography--title-color-contrast');
var _debois$elm_mdl$Material_Typography$headline = _debois$elm_mdl$Material_Options$cs('mdl-typography--headline-color-contrast');
var _debois$elm_mdl$Material_Typography$display4 = _debois$elm_mdl$Material_Options$cs('mdl-typography--display-4-color-contrast');
var _debois$elm_mdl$Material_Typography$display3 = _debois$elm_mdl$Material_Options$cs('mdl-typography--display-3-color-contrast');
var _debois$elm_mdl$Material_Typography$display2 = _debois$elm_mdl$Material_Options$cs('mdl-typography--display-2-color-contrast');
var _debois$elm_mdl$Material_Typography$display1 = _debois$elm_mdl$Material_Options$cs('mdl-typography--display-1-color-contrast');

var _elm_lang$core$Set$foldr = F3(
	function (f, b, _p0) {
		var _p1 = _p0;
		return A3(
			_elm_lang$core$Dict$foldr,
			F3(
				function (k, _p2, b) {
					return A2(f, k, b);
				}),
			b,
			_p1._0);
	});
var _elm_lang$core$Set$foldl = F3(
	function (f, b, _p3) {
		var _p4 = _p3;
		return A3(
			_elm_lang$core$Dict$foldl,
			F3(
				function (k, _p5, b) {
					return A2(f, k, b);
				}),
			b,
			_p4._0);
	});
var _elm_lang$core$Set$toList = function (_p6) {
	var _p7 = _p6;
	return _elm_lang$core$Dict$keys(_p7._0);
};
var _elm_lang$core$Set$size = function (_p8) {
	var _p9 = _p8;
	return _elm_lang$core$Dict$size(_p9._0);
};
var _elm_lang$core$Set$member = F2(
	function (k, _p10) {
		var _p11 = _p10;
		return A2(_elm_lang$core$Dict$member, k, _p11._0);
	});
var _elm_lang$core$Set$isEmpty = function (_p12) {
	var _p13 = _p12;
	return _elm_lang$core$Dict$isEmpty(_p13._0);
};
var _elm_lang$core$Set$Set_elm_builtin = function (a) {
	return {ctor: 'Set_elm_builtin', _0: a};
};
var _elm_lang$core$Set$empty = _elm_lang$core$Set$Set_elm_builtin(_elm_lang$core$Dict$empty);
var _elm_lang$core$Set$singleton = function (k) {
	return _elm_lang$core$Set$Set_elm_builtin(
		A2(
			_elm_lang$core$Dict$singleton,
			k,
			{ctor: '_Tuple0'}));
};
var _elm_lang$core$Set$insert = F2(
	function (k, _p14) {
		var _p15 = _p14;
		return _elm_lang$core$Set$Set_elm_builtin(
			A3(
				_elm_lang$core$Dict$insert,
				k,
				{ctor: '_Tuple0'},
				_p15._0));
	});
var _elm_lang$core$Set$fromList = function (xs) {
	return A3(_elm_lang$core$List$foldl, _elm_lang$core$Set$insert, _elm_lang$core$Set$empty, xs);
};
var _elm_lang$core$Set$map = F2(
	function (f, s) {
		return _elm_lang$core$Set$fromList(
			A2(
				_elm_lang$core$List$map,
				f,
				_elm_lang$core$Set$toList(s)));
	});
var _elm_lang$core$Set$remove = F2(
	function (k, _p16) {
		var _p17 = _p16;
		return _elm_lang$core$Set$Set_elm_builtin(
			A2(_elm_lang$core$Dict$remove, k, _p17._0));
	});
var _elm_lang$core$Set$union = F2(
	function (_p19, _p18) {
		var _p20 = _p19;
		var _p21 = _p18;
		return _elm_lang$core$Set$Set_elm_builtin(
			A2(_elm_lang$core$Dict$union, _p20._0, _p21._0));
	});
var _elm_lang$core$Set$intersect = F2(
	function (_p23, _p22) {
		var _p24 = _p23;
		var _p25 = _p22;
		return _elm_lang$core$Set$Set_elm_builtin(
			A2(_elm_lang$core$Dict$intersect, _p24._0, _p25._0));
	});
var _elm_lang$core$Set$diff = F2(
	function (_p27, _p26) {
		var _p28 = _p27;
		var _p29 = _p26;
		return _elm_lang$core$Set$Set_elm_builtin(
			A2(_elm_lang$core$Dict$diff, _p28._0, _p29._0));
	});
var _elm_lang$core$Set$filter = F2(
	function (p, _p30) {
		var _p31 = _p30;
		return _elm_lang$core$Set$Set_elm_builtin(
			A2(
				_elm_lang$core$Dict$filter,
				F2(
					function (k, _p32) {
						return p(k);
					}),
				_p31._0));
	});
var _elm_lang$core$Set$partition = F2(
	function (p, _p33) {
		var _p34 = _p33;
		var _p35 = A2(
			_elm_lang$core$Dict$partition,
			F2(
				function (k, _p36) {
					return p(k);
				}),
			_p34._0);
		var p1 = _p35._0;
		var p2 = _p35._1;
		return {
			ctor: '_Tuple2',
			_0: _elm_lang$core$Set$Set_elm_builtin(p1),
			_1: _elm_lang$core$Set$Set_elm_builtin(p2)
		};
	});

var _elm_community$list_extra$List_Extra$greedyGroupsOfWithStep = F3(
	function (size, step, xs) {
		var okayXs = _elm_lang$core$Native_Utils.cmp(
			_elm_lang$core$List$length(xs),
			0) > 0;
		var okayArgs = (_elm_lang$core$Native_Utils.cmp(size, 0) > 0) && (_elm_lang$core$Native_Utils.cmp(step, 0) > 0);
		var xs_ = A2(_elm_lang$core$List$drop, step, xs);
		var group = A2(_elm_lang$core$List$take, size, xs);
		return (okayArgs && okayXs) ? {
			ctor: '::',
			_0: group,
			_1: A3(_elm_community$list_extra$List_Extra$greedyGroupsOfWithStep, size, step, xs_)
		} : {ctor: '[]'};
	});
var _elm_community$list_extra$List_Extra$greedyGroupsOf = F2(
	function (size, xs) {
		return A3(_elm_community$list_extra$List_Extra$greedyGroupsOfWithStep, size, size, xs);
	});
var _elm_community$list_extra$List_Extra$groupsOfWithStep = F3(
	function (size, step, xs) {
		var okayArgs = (_elm_lang$core$Native_Utils.cmp(size, 0) > 0) && (_elm_lang$core$Native_Utils.cmp(step, 0) > 0);
		var xs_ = A2(_elm_lang$core$List$drop, step, xs);
		var group = A2(_elm_lang$core$List$take, size, xs);
		var okayLength = _elm_lang$core$Native_Utils.eq(
			size,
			_elm_lang$core$List$length(group));
		return (okayArgs && okayLength) ? {
			ctor: '::',
			_0: group,
			_1: A3(_elm_community$list_extra$List_Extra$groupsOfWithStep, size, step, xs_)
		} : {ctor: '[]'};
	});
var _elm_community$list_extra$List_Extra$groupsOf = F2(
	function (size, xs) {
		return A3(_elm_community$list_extra$List_Extra$groupsOfWithStep, size, size, xs);
	});
var _elm_community$list_extra$List_Extra$zip5 = _elm_lang$core$List$map5(
	F5(
		function (v0, v1, v2, v3, v4) {
			return {ctor: '_Tuple5', _0: v0, _1: v1, _2: v2, _3: v3, _4: v4};
		}));
var _elm_community$list_extra$List_Extra$zip4 = _elm_lang$core$List$map4(
	F4(
		function (v0, v1, v2, v3) {
			return {ctor: '_Tuple4', _0: v0, _1: v1, _2: v2, _3: v3};
		}));
var _elm_community$list_extra$List_Extra$zip3 = _elm_lang$core$List$map3(
	F3(
		function (v0, v1, v2) {
			return {ctor: '_Tuple3', _0: v0, _1: v1, _2: v2};
		}));
var _elm_community$list_extra$List_Extra$zip = _elm_lang$core$List$map2(
	F2(
		function (v0, v1) {
			return {ctor: '_Tuple2', _0: v0, _1: v1};
		}));
var _elm_community$list_extra$List_Extra$isSubsequenceOf = F2(
	function (subseq, list) {
		isSubsequenceOf:
		while (true) {
			var _p0 = {ctor: '_Tuple2', _0: subseq, _1: list};
			if (_p0._0.ctor === '[]') {
				return true;
			} else {
				if (_p0._1.ctor === '[]') {
					return false;
				} else {
					var _p1 = _p0._1._1;
					if (_elm_lang$core$Native_Utils.eq(_p0._0._0, _p0._1._0)) {
						var _v1 = _p0._0._1,
							_v2 = _p1;
						subseq = _v1;
						list = _v2;
						continue isSubsequenceOf;
					} else {
						var _v3 = subseq,
							_v4 = _p1;
						subseq = _v3;
						list = _v4;
						continue isSubsequenceOf;
					}
				}
			}
		}
	});
var _elm_community$list_extra$List_Extra$isPrefixOf = F2(
	function (prefix, xs) {
		var _p2 = {ctor: '_Tuple2', _0: prefix, _1: xs};
		if (_p2._0.ctor === '[]') {
			return true;
		} else {
			if (_p2._1.ctor === '[]') {
				return false;
			} else {
				return _elm_lang$core$Native_Utils.eq(_p2._0._0, _p2._1._0) && A2(_elm_community$list_extra$List_Extra$isPrefixOf, _p2._0._1, _p2._1._1);
			}
		}
	});
var _elm_community$list_extra$List_Extra$isSuffixOf = F2(
	function (suffix, xs) {
		return A2(
			_elm_community$list_extra$List_Extra$isPrefixOf,
			_elm_lang$core$List$reverse(suffix),
			_elm_lang$core$List$reverse(xs));
	});
var _elm_community$list_extra$List_Extra$isInfixOfHelp = F3(
	function (infixHead, infixTail, list) {
		isInfixOfHelp:
		while (true) {
			var _p3 = list;
			if (_p3.ctor === '[]') {
				return false;
			} else {
				var _p4 = _p3._1;
				if (_elm_lang$core$Native_Utils.eq(_p3._0, infixHead)) {
					return A2(_elm_community$list_extra$List_Extra$isPrefixOf, infixTail, _p4);
				} else {
					var _v7 = infixHead,
						_v8 = infixTail,
						_v9 = _p4;
					infixHead = _v7;
					infixTail = _v8;
					list = _v9;
					continue isInfixOfHelp;
				}
			}
		}
	});
var _elm_community$list_extra$List_Extra$isInfixOf = F2(
	function (infixList, list) {
		var _p5 = infixList;
		if (_p5.ctor === '[]') {
			return true;
		} else {
			return A3(_elm_community$list_extra$List_Extra$isInfixOfHelp, _p5._0, _p5._1, list);
		}
	});
var _elm_community$list_extra$List_Extra$selectSplit = function (xs) {
	var _p6 = xs;
	if (_p6.ctor === '[]') {
		return {ctor: '[]'};
	} else {
		var _p10 = _p6._1;
		var _p9 = _p6._0;
		return {
			ctor: '::',
			_0: {
				ctor: '_Tuple3',
				_0: {ctor: '[]'},
				_1: _p9,
				_2: _p10
			},
			_1: A2(
				_elm_lang$core$List$map,
				function (_p7) {
					var _p8 = _p7;
					return {
						ctor: '_Tuple3',
						_0: {ctor: '::', _0: _p9, _1: _p8._0},
						_1: _p8._1,
						_2: _p8._2
					};
				},
				_elm_community$list_extra$List_Extra$selectSplit(_p10))
		};
	}
};
var _elm_community$list_extra$List_Extra$select = function (xs) {
	var _p11 = xs;
	if (_p11.ctor === '[]') {
		return {ctor: '[]'};
	} else {
		var _p15 = _p11._1;
		var _p14 = _p11._0;
		return {
			ctor: '::',
			_0: {ctor: '_Tuple2', _0: _p14, _1: _p15},
			_1: A2(
				_elm_lang$core$List$map,
				function (_p12) {
					var _p13 = _p12;
					return {
						ctor: '_Tuple2',
						_0: _p13._0,
						_1: {ctor: '::', _0: _p14, _1: _p13._1}
					};
				},
				_elm_community$list_extra$List_Extra$select(_p15))
		};
	}
};
var _elm_community$list_extra$List_Extra$tailsHelp = F2(
	function (e, list) {
		var _p16 = list;
		if (_p16.ctor === '::') {
			var _p17 = _p16._0;
			return {
				ctor: '::',
				_0: {ctor: '::', _0: e, _1: _p17},
				_1: {ctor: '::', _0: _p17, _1: _p16._1}
			};
		} else {
			return {ctor: '[]'};
		}
	});
var _elm_community$list_extra$List_Extra$tails = A2(
	_elm_lang$core$List$foldr,
	_elm_community$list_extra$List_Extra$tailsHelp,
	{
		ctor: '::',
		_0: {ctor: '[]'},
		_1: {ctor: '[]'}
	});
var _elm_community$list_extra$List_Extra$inits = A2(
	_elm_lang$core$List$foldr,
	F2(
		function (e, acc) {
			return {
				ctor: '::',
				_0: {ctor: '[]'},
				_1: A2(
					_elm_lang$core$List$map,
					F2(
						function (x, y) {
							return {ctor: '::', _0: x, _1: y};
						})(e),
					acc)
			};
		}),
	{
		ctor: '::',
		_0: {ctor: '[]'},
		_1: {ctor: '[]'}
	});
var _elm_community$list_extra$List_Extra$groupWhileTransitivelyHelp = F4(
	function (result, currentGroup, compare, list) {
		groupWhileTransitivelyHelp:
		while (true) {
			var _p18 = list;
			if (_p18.ctor === '[]') {
				return _elm_lang$core$List$reverse(
					_elm_lang$core$List$isEmpty(currentGroup) ? result : _elm_lang$core$List$reverse(
						{ctor: '::', _0: currentGroup, _1: result}));
			} else {
				if (_p18._1.ctor === '[]') {
					return _elm_lang$core$List$reverse(
						{
							ctor: '::',
							_0: _elm_lang$core$List$reverse(
								{ctor: '::', _0: _p18._0, _1: currentGroup}),
							_1: result
						});
				} else {
					var _p20 = _p18._1;
					var _p19 = _p18._0;
					if (A2(compare, _p19, _p18._1._0)) {
						var _v17 = result,
							_v18 = {ctor: '::', _0: _p19, _1: currentGroup},
							_v19 = compare,
							_v20 = _p20;
						result = _v17;
						currentGroup = _v18;
						compare = _v19;
						list = _v20;
						continue groupWhileTransitivelyHelp;
					} else {
						var _v21 = {
							ctor: '::',
							_0: _elm_lang$core$List$reverse(
								{ctor: '::', _0: _p19, _1: currentGroup}),
							_1: result
						},
							_v22 = {ctor: '[]'},
							_v23 = compare,
							_v24 = _p20;
						result = _v21;
						currentGroup = _v22;
						compare = _v23;
						list = _v24;
						continue groupWhileTransitivelyHelp;
					}
				}
			}
		}
	});
var _elm_community$list_extra$List_Extra$groupWhileTransitively = F2(
	function (compare, list) {
		return A4(
			_elm_community$list_extra$List_Extra$groupWhileTransitivelyHelp,
			{ctor: '[]'},
			{ctor: '[]'},
			compare,
			list);
	});
var _elm_community$list_extra$List_Extra$stripPrefix = F2(
	function (prefix, xs) {
		var step = F2(
			function (e, m) {
				var _p21 = m;
				if (_p21.ctor === 'Nothing') {
					return _elm_lang$core$Maybe$Nothing;
				} else {
					if (_p21._0.ctor === '[]') {
						return _elm_lang$core$Maybe$Nothing;
					} else {
						return _elm_lang$core$Native_Utils.eq(e, _p21._0._0) ? _elm_lang$core$Maybe$Just(_p21._0._1) : _elm_lang$core$Maybe$Nothing;
					}
				}
			});
		return A3(
			_elm_lang$core$List$foldl,
			step,
			_elm_lang$core$Maybe$Just(xs),
			prefix);
	});
var _elm_community$list_extra$List_Extra$dropWhileRight = function (p) {
	return A2(
		_elm_lang$core$List$foldr,
		F2(
			function (x, xs) {
				return (p(x) && _elm_lang$core$List$isEmpty(xs)) ? {ctor: '[]'} : {ctor: '::', _0: x, _1: xs};
			}),
		{ctor: '[]'});
};
var _elm_community$list_extra$List_Extra$takeWhileRight = function (p) {
	var step = F2(
		function (x, _p22) {
			var _p23 = _p22;
			var _p24 = _p23._0;
			return (p(x) && _p23._1) ? {
				ctor: '_Tuple2',
				_0: {ctor: '::', _0: x, _1: _p24},
				_1: true
			} : {ctor: '_Tuple2', _0: _p24, _1: false};
		});
	return function (_p25) {
		return _elm_lang$core$Tuple$first(
			A3(
				_elm_lang$core$List$foldr,
				step,
				{
					ctor: '_Tuple2',
					_0: {ctor: '[]'},
					_1: true
				},
				_p25));
	};
};
var _elm_community$list_extra$List_Extra$splitAt = F2(
	function (n, xs) {
		return {
			ctor: '_Tuple2',
			_0: A2(_elm_lang$core$List$take, n, xs),
			_1: A2(_elm_lang$core$List$drop, n, xs)
		};
	});
var _elm_community$list_extra$List_Extra$groupsOfVarying_ = F3(
	function (listOflengths, list, accu) {
		groupsOfVarying_:
		while (true) {
			var _p26 = {ctor: '_Tuple2', _0: listOflengths, _1: list};
			if (((_p26.ctor === '_Tuple2') && (_p26._0.ctor === '::')) && (_p26._1.ctor === '::')) {
				var _p27 = A2(_elm_community$list_extra$List_Extra$splitAt, _p26._0._0, list);
				var head = _p27._0;
				var tail = _p27._1;
				var _v28 = _p26._0._1,
					_v29 = tail,
					_v30 = {ctor: '::', _0: head, _1: accu};
				listOflengths = _v28;
				list = _v29;
				accu = _v30;
				continue groupsOfVarying_;
			} else {
				return _elm_lang$core$List$reverse(accu);
			}
		}
	});
var _elm_community$list_extra$List_Extra$groupsOfVarying = F2(
	function (listOflengths, list) {
		return A3(
			_elm_community$list_extra$List_Extra$groupsOfVarying_,
			listOflengths,
			list,
			{ctor: '[]'});
	});
var _elm_community$list_extra$List_Extra$unfoldr = F2(
	function (f, seed) {
		var _p28 = f(seed);
		if (_p28.ctor === 'Nothing') {
			return {ctor: '[]'};
		} else {
			return {
				ctor: '::',
				_0: _p28._0._0,
				_1: A2(_elm_community$list_extra$List_Extra$unfoldr, f, _p28._0._1)
			};
		}
	});
var _elm_community$list_extra$List_Extra$mapAccumr = F3(
	function (f, acc0, list) {
		return A3(
			_elm_lang$core$List$foldr,
			F2(
				function (x, _p29) {
					var _p30 = _p29;
					var _p31 = A2(f, _p30._0, x);
					var acc2 = _p31._0;
					var y = _p31._1;
					return {
						ctor: '_Tuple2',
						_0: acc2,
						_1: {ctor: '::', _0: y, _1: _p30._1}
					};
				}),
			{
				ctor: '_Tuple2',
				_0: acc0,
				_1: {ctor: '[]'}
			},
			list);
	});
var _elm_community$list_extra$List_Extra$mapAccuml = F3(
	function (f, acc0, list) {
		var _p32 = A3(
			_elm_lang$core$List$foldl,
			F2(
				function (x, _p33) {
					var _p34 = _p33;
					var _p35 = A2(f, _p34._0, x);
					var acc2 = _p35._0;
					var y = _p35._1;
					return {
						ctor: '_Tuple2',
						_0: acc2,
						_1: {ctor: '::', _0: y, _1: _p34._1}
					};
				}),
			{
				ctor: '_Tuple2',
				_0: acc0,
				_1: {ctor: '[]'}
			},
			list);
		var accFinal = _p32._0;
		var generatedList = _p32._1;
		return {
			ctor: '_Tuple2',
			_0: accFinal,
			_1: _elm_lang$core$List$reverse(generatedList)
		};
	});
var _elm_community$list_extra$List_Extra$scanr1 = F2(
	function (f, xs_) {
		var _p36 = xs_;
		if (_p36.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			if (_p36._1.ctor === '[]') {
				return {
					ctor: '::',
					_0: _p36._0,
					_1: {ctor: '[]'}
				};
			} else {
				var _p37 = A2(_elm_community$list_extra$List_Extra$scanr1, f, _p36._1);
				if (_p37.ctor === '::') {
					return {
						ctor: '::',
						_0: A2(f, _p36._0, _p37._0),
						_1: _p37
					};
				} else {
					return {ctor: '[]'};
				}
			}
		}
	});
var _elm_community$list_extra$List_Extra$scanr = F3(
	function (f, acc, xs_) {
		var _p38 = xs_;
		if (_p38.ctor === '[]') {
			return {
				ctor: '::',
				_0: acc,
				_1: {ctor: '[]'}
			};
		} else {
			var _p39 = A3(_elm_community$list_extra$List_Extra$scanr, f, acc, _p38._1);
			if (_p39.ctor === '::') {
				return {
					ctor: '::',
					_0: A2(f, _p38._0, _p39._0),
					_1: _p39
				};
			} else {
				return {ctor: '[]'};
			}
		}
	});
var _elm_community$list_extra$List_Extra$scanl1 = F2(
	function (f, xs_) {
		var _p40 = xs_;
		if (_p40.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			return A3(_elm_lang$core$List$scanl, f, _p40._0, _p40._1);
		}
	});
var _elm_community$list_extra$List_Extra$indexedFoldr = F3(
	function (func, acc, list) {
		var step = F2(
			function (x, _p41) {
				var _p42 = _p41;
				var _p43 = _p42._0;
				return {
					ctor: '_Tuple2',
					_0: _p43 - 1,
					_1: A3(func, _p43, x, _p42._1)
				};
			});
		return _elm_lang$core$Tuple$second(
			A3(
				_elm_lang$core$List$foldr,
				step,
				{
					ctor: '_Tuple2',
					_0: _elm_lang$core$List$length(list) - 1,
					_1: acc
				},
				list));
	});
var _elm_community$list_extra$List_Extra$indexedFoldl = F3(
	function (func, acc, list) {
		var step = F2(
			function (x, _p44) {
				var _p45 = _p44;
				var _p46 = _p45._0;
				return {
					ctor: '_Tuple2',
					_0: _p46 + 1,
					_1: A3(func, _p46, x, _p45._1)
				};
			});
		return _elm_lang$core$Tuple$second(
			A3(
				_elm_lang$core$List$foldl,
				step,
				{ctor: '_Tuple2', _0: 0, _1: acc},
				list));
	});
var _elm_community$list_extra$List_Extra$foldr1 = F2(
	function (f, xs) {
		var mf = F2(
			function (x, m) {
				return _elm_lang$core$Maybe$Just(
					function () {
						var _p47 = m;
						if (_p47.ctor === 'Nothing') {
							return x;
						} else {
							return A2(f, x, _p47._0);
						}
					}());
			});
		return A3(_elm_lang$core$List$foldr, mf, _elm_lang$core$Maybe$Nothing, xs);
	});
var _elm_community$list_extra$List_Extra$foldl1 = F2(
	function (f, xs) {
		var mf = F2(
			function (x, m) {
				return _elm_lang$core$Maybe$Just(
					function () {
						var _p48 = m;
						if (_p48.ctor === 'Nothing') {
							return x;
						} else {
							return A2(f, _p48._0, x);
						}
					}());
			});
		return A3(_elm_lang$core$List$foldl, mf, _elm_lang$core$Maybe$Nothing, xs);
	});
var _elm_community$list_extra$List_Extra$reverseAppend = F2(
	function (list1, list2) {
		return A3(
			_elm_lang$core$List$foldl,
			F2(
				function (x, y) {
					return {ctor: '::', _0: x, _1: y};
				}),
			list2,
			list1);
	});
var _elm_community$list_extra$List_Extra$interweaveHelp = F3(
	function (acc, list1, list2) {
		interweaveHelp:
		while (true) {
			var _p49 = {ctor: '_Tuple2', _0: list1, _1: list2};
			if (_p49._0.ctor === '::') {
				if (_p49._1.ctor === '::') {
					var _v44 = {
						ctor: '::',
						_0: _p49._1._0,
						_1: {ctor: '::', _0: _p49._0._0, _1: acc}
					},
						_v45 = _p49._0._1,
						_v46 = _p49._1._1;
					acc = _v44;
					list1 = _v45;
					list2 = _v46;
					continue interweaveHelp;
				} else {
					return A2(_elm_community$list_extra$List_Extra$reverseAppend, acc, list1);
				}
			} else {
				return A2(_elm_community$list_extra$List_Extra$reverseAppend, acc, list2);
			}
		}
	});
var _elm_community$list_extra$List_Extra$interweave = _elm_community$list_extra$List_Extra$interweaveHelp(
	{ctor: '[]'});
var _elm_community$list_extra$List_Extra$permutations = function (xs_) {
	var _p50 = xs_;
	if (_p50.ctor === '[]') {
		return {
			ctor: '::',
			_0: {ctor: '[]'},
			_1: {ctor: '[]'}
		};
	} else {
		var f = function (_p51) {
			var _p52 = _p51;
			return A2(
				_elm_lang$core$List$map,
				F2(
					function (x, y) {
						return {ctor: '::', _0: x, _1: y};
					})(_p52._0),
				_elm_community$list_extra$List_Extra$permutations(_p52._1));
		};
		return A2(
			_elm_lang$core$List$concatMap,
			f,
			_elm_community$list_extra$List_Extra$select(_p50));
	}
};
var _elm_community$list_extra$List_Extra$isPermutationOf = F2(
	function (permut, xs) {
		return A2(
			_elm_lang$core$List$member,
			permut,
			_elm_community$list_extra$List_Extra$permutations(xs));
	});
var _elm_community$list_extra$List_Extra$subsequencesNonEmpty = function (xs) {
	var _p53 = xs;
	if (_p53.ctor === '[]') {
		return {ctor: '[]'};
	} else {
		var _p54 = _p53._0;
		var f = F2(
			function (ys, r) {
				return {
					ctor: '::',
					_0: ys,
					_1: {
						ctor: '::',
						_0: {ctor: '::', _0: _p54, _1: ys},
						_1: r
					}
				};
			});
		return {
			ctor: '::',
			_0: {
				ctor: '::',
				_0: _p54,
				_1: {ctor: '[]'}
			},
			_1: A3(
				_elm_lang$core$List$foldr,
				f,
				{ctor: '[]'},
				_elm_community$list_extra$List_Extra$subsequencesNonEmpty(_p53._1))
		};
	}
};
var _elm_community$list_extra$List_Extra$subsequences = function (xs) {
	return {
		ctor: '::',
		_0: {ctor: '[]'},
		_1: _elm_community$list_extra$List_Extra$subsequencesNonEmpty(xs)
	};
};
var _elm_community$list_extra$List_Extra$rowsLength = function (listOfLists) {
	var _p55 = listOfLists;
	if (_p55.ctor === '[]') {
		return 0;
	} else {
		return _elm_lang$core$List$length(_p55._0);
	}
};
var _elm_community$list_extra$List_Extra$transpose = function (listOfLists) {
	return A3(
		_elm_lang$core$List$foldr,
		_elm_lang$core$List$map2(
			F2(
				function (x, y) {
					return {ctor: '::', _0: x, _1: y};
				})),
		A2(
			_elm_lang$core$List$repeat,
			_elm_community$list_extra$List_Extra$rowsLength(listOfLists),
			{ctor: '[]'}),
		listOfLists);
};
var _elm_community$list_extra$List_Extra$intercalate = function (xs) {
	return function (_p56) {
		return _elm_lang$core$List$concat(
			A2(_elm_lang$core$List$intersperse, xs, _p56));
	};
};
var _elm_community$list_extra$List_Extra$filterNot = F2(
	function (pred, list) {
		return A2(
			_elm_lang$core$List$filter,
			function (_p57) {
				return !pred(_p57);
			},
			list);
	});
var _elm_community$list_extra$List_Extra$removeIfIndex = function (predicate) {
	return A2(
		_elm_community$list_extra$List_Extra$indexedFoldr,
		F3(
			function (index, item, acc) {
				return predicate(index) ? acc : {ctor: '::', _0: item, _1: acc};
			}),
		{ctor: '[]'});
};
var _elm_community$list_extra$List_Extra$removeAt = F2(
	function (index, l) {
		if (_elm_lang$core$Native_Utils.cmp(index, 0) < 0) {
			return l;
		} else {
			var tail = _elm_lang$core$List$tail(
				A2(_elm_lang$core$List$drop, index, l));
			var head = A2(_elm_lang$core$List$take, index, l);
			var _p58 = tail;
			if (_p58.ctor === 'Nothing') {
				return l;
			} else {
				return A2(_elm_lang$core$List$append, head, _p58._0);
			}
		}
	});
var _elm_community$list_extra$List_Extra$stableSortWith = F2(
	function (pred, list) {
		var predWithIndex = F2(
			function (_p60, _p59) {
				var _p61 = _p60;
				var _p62 = _p59;
				var result = A2(pred, _p61._0, _p62._0);
				var _p63 = result;
				if (_p63.ctor === 'EQ') {
					return A2(_elm_lang$core$Basics$compare, _p61._1, _p62._1);
				} else {
					return result;
				}
			});
		var listWithIndex = A2(
			_elm_lang$core$List$indexedMap,
			F2(
				function (i, a) {
					return {ctor: '_Tuple2', _0: a, _1: i};
				}),
			list);
		return A2(
			_elm_lang$core$List$map,
			_elm_lang$core$Tuple$first,
			A2(_elm_lang$core$List$sortWith, predWithIndex, listWithIndex));
	});
var _elm_community$list_extra$List_Extra$remove = F2(
	function (x, xs) {
		var _p64 = xs;
		if (_p64.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			var _p66 = _p64._1;
			var _p65 = _p64._0;
			return _elm_lang$core$Native_Utils.eq(x, _p65) ? _p66 : {
				ctor: '::',
				_0: _p65,
				_1: A2(_elm_community$list_extra$List_Extra$remove, x, _p66)
			};
		}
	});
var _elm_community$list_extra$List_Extra$updateIfIndex = F3(
	function (predicate, update, list) {
		return A2(
			_elm_lang$core$List$indexedMap,
			F2(
				function (i, x) {
					return predicate(i) ? update(x) : x;
				}),
			list);
	});
var _elm_community$list_extra$List_Extra$updateAt = F3(
	function (index, fn, list) {
		if (_elm_lang$core$Native_Utils.cmp(index, 0) < 0) {
			return list;
		} else {
			var tail = A2(_elm_lang$core$List$drop, index, list);
			var head = A2(_elm_lang$core$List$take, index, list);
			var _p67 = tail;
			if (_p67.ctor === '::') {
				return A2(
					_elm_lang$core$Basics_ops['++'],
					head,
					{
						ctor: '::',
						_0: fn(_p67._0),
						_1: _p67._1
					});
			} else {
				return list;
			}
		}
	});
var _elm_community$list_extra$List_Extra$setAt = F2(
	function (index, value) {
		return A2(
			_elm_community$list_extra$List_Extra$updateAt,
			index,
			_elm_lang$core$Basics$always(value));
	});
var _elm_community$list_extra$List_Extra$updateIf = F3(
	function (predicate, update, list) {
		return A2(
			_elm_lang$core$List$map,
			function (item) {
				return predicate(item) ? update(item) : item;
			},
			list);
	});
var _elm_community$list_extra$List_Extra$replaceIf = F3(
	function (predicate, replacement, list) {
		return A3(
			_elm_community$list_extra$List_Extra$updateIf,
			predicate,
			_elm_lang$core$Basics$always(replacement),
			list);
	});
var _elm_community$list_extra$List_Extra$count = function (predicate) {
	return A2(
		_elm_lang$core$List$foldl,
		F2(
			function (x, acc) {
				return predicate(x) ? (acc + 1) : acc;
			}),
		0);
};
var _elm_community$list_extra$List_Extra$findIndices = function (predicate) {
	var consIndexIf = F3(
		function (index, x, acc) {
			return predicate(x) ? {ctor: '::', _0: index, _1: acc} : acc;
		});
	return A2(
		_elm_community$list_extra$List_Extra$indexedFoldr,
		consIndexIf,
		{ctor: '[]'});
};
var _elm_community$list_extra$List_Extra$findIndexHelp = F3(
	function (index, predicate, list) {
		findIndexHelp:
		while (true) {
			var _p68 = list;
			if (_p68.ctor === '[]') {
				return _elm_lang$core$Maybe$Nothing;
			} else {
				if (predicate(_p68._0)) {
					return _elm_lang$core$Maybe$Just(index);
				} else {
					var _v58 = index + 1,
						_v59 = predicate,
						_v60 = _p68._1;
					index = _v58;
					predicate = _v59;
					list = _v60;
					continue findIndexHelp;
				}
			}
		}
	});
var _elm_community$list_extra$List_Extra$findIndex = _elm_community$list_extra$List_Extra$findIndexHelp(0);
var _elm_community$list_extra$List_Extra$splitWhen = F2(
	function (predicate, list) {
		return A2(
			_elm_lang$core$Maybe$map,
			function (i) {
				return A2(_elm_community$list_extra$List_Extra$splitAt, i, list);
			},
			A2(_elm_community$list_extra$List_Extra$findIndex, predicate, list));
	});
var _elm_community$list_extra$List_Extra$elemIndices = function (x) {
	return _elm_community$list_extra$List_Extra$findIndices(
		F2(
			function (x, y) {
				return _elm_lang$core$Native_Utils.eq(x, y);
			})(x));
};
var _elm_community$list_extra$List_Extra$elemIndex = function (x) {
	return _elm_community$list_extra$List_Extra$findIndex(
		F2(
			function (x, y) {
				return _elm_lang$core$Native_Utils.eq(x, y);
			})(x));
};
var _elm_community$list_extra$List_Extra$find = F2(
	function (predicate, list) {
		find:
		while (true) {
			var _p69 = list;
			if (_p69.ctor === '[]') {
				return _elm_lang$core$Maybe$Nothing;
			} else {
				var _p70 = _p69._0;
				if (predicate(_p70)) {
					return _elm_lang$core$Maybe$Just(_p70);
				} else {
					var _v62 = predicate,
						_v63 = _p69._1;
					predicate = _v62;
					list = _v63;
					continue find;
				}
			}
		}
	});
var _elm_community$list_extra$List_Extra$notMember = function (x) {
	return function (_p71) {
		return !A2(_elm_lang$core$List$member, x, _p71);
	};
};
var _elm_community$list_extra$List_Extra$reverseMap = F2(
	function (f, xs) {
		return A3(
			_elm_lang$core$List$foldl,
			F2(
				function (x, acc) {
					return {
						ctor: '::',
						_0: f(x),
						_1: acc
					};
				}),
			{ctor: '[]'},
			xs);
	});
var _elm_community$list_extra$List_Extra$andThen = _elm_lang$core$List$concatMap;
var _elm_community$list_extra$List_Extra$lift2 = F3(
	function (f, la, lb) {
		return A2(
			_elm_community$list_extra$List_Extra$andThen,
			function (a) {
				return A2(
					_elm_community$list_extra$List_Extra$andThen,
					function (b) {
						return {
							ctor: '::',
							_0: A2(f, a, b),
							_1: {ctor: '[]'}
						};
					},
					lb);
			},
			la);
	});
var _elm_community$list_extra$List_Extra$cartesianProduct = function (ll) {
	var _p72 = ll;
	if (_p72.ctor === '[]') {
		return {
			ctor: '::',
			_0: {ctor: '[]'},
			_1: {ctor: '[]'}
		};
	} else {
		return A3(
			_elm_community$list_extra$List_Extra$lift2,
			F2(
				function (x, y) {
					return {ctor: '::', _0: x, _1: y};
				}),
			_p72._0,
			_elm_community$list_extra$List_Extra$cartesianProduct(_p72._1));
	}
};
var _elm_community$list_extra$List_Extra$lift3 = F4(
	function (f, la, lb, lc) {
		return A2(
			_elm_community$list_extra$List_Extra$andThen,
			function (a) {
				return A2(
					_elm_community$list_extra$List_Extra$andThen,
					function (b) {
						return A2(
							_elm_community$list_extra$List_Extra$andThen,
							function (c) {
								return {
									ctor: '::',
									_0: A3(f, a, b, c),
									_1: {ctor: '[]'}
								};
							},
							lc);
					},
					lb);
			},
			la);
	});
var _elm_community$list_extra$List_Extra$lift4 = F5(
	function (f, la, lb, lc, ld) {
		return A2(
			_elm_community$list_extra$List_Extra$andThen,
			function (a) {
				return A2(
					_elm_community$list_extra$List_Extra$andThen,
					function (b) {
						return A2(
							_elm_community$list_extra$List_Extra$andThen,
							function (c) {
								return A2(
									_elm_community$list_extra$List_Extra$andThen,
									function (d) {
										return {
											ctor: '::',
											_0: A4(f, a, b, c, d),
											_1: {ctor: '[]'}
										};
									},
									ld);
							},
							lc);
					},
					lb);
			},
			la);
	});
var _elm_community$list_extra$List_Extra$andMap = F2(
	function (l, fl) {
		return A3(
			_elm_lang$core$List$map2,
			F2(
				function (x, y) {
					return x(y);
				}),
			fl,
			l);
	});
var _elm_community$list_extra$List_Extra$uniqueHelp = F4(
	function (f, existing, remaining, accumulator) {
		uniqueHelp:
		while (true) {
			var _p73 = remaining;
			if (_p73.ctor === '[]') {
				return _elm_lang$core$List$reverse(accumulator);
			} else {
				var _p75 = _p73._1;
				var _p74 = _p73._0;
				var computedFirst = f(_p74);
				if (A2(_elm_lang$core$Set$member, computedFirst, existing)) {
					var _v66 = f,
						_v67 = existing,
						_v68 = _p75,
						_v69 = accumulator;
					f = _v66;
					existing = _v67;
					remaining = _v68;
					accumulator = _v69;
					continue uniqueHelp;
				} else {
					var _v70 = f,
						_v71 = A2(_elm_lang$core$Set$insert, computedFirst, existing),
						_v72 = _p75,
						_v73 = {ctor: '::', _0: _p74, _1: accumulator};
					f = _v70;
					existing = _v71;
					remaining = _v72;
					accumulator = _v73;
					continue uniqueHelp;
				}
			}
		}
	});
var _elm_community$list_extra$List_Extra$uniqueBy = F2(
	function (f, list) {
		return A4(
			_elm_community$list_extra$List_Extra$uniqueHelp,
			f,
			_elm_lang$core$Set$empty,
			list,
			{ctor: '[]'});
	});
var _elm_community$list_extra$List_Extra$allDifferentBy = F2(
	function (f, list) {
		return _elm_lang$core$Native_Utils.eq(
			_elm_lang$core$List$length(list),
			_elm_lang$core$List$length(
				A2(_elm_community$list_extra$List_Extra$uniqueBy, f, list)));
	});
var _elm_community$list_extra$List_Extra$allDifferent = function (list) {
	return A2(_elm_community$list_extra$List_Extra$allDifferentBy, _elm_lang$core$Basics$identity, list);
};
var _elm_community$list_extra$List_Extra$unique = function (list) {
	return A4(
		_elm_community$list_extra$List_Extra$uniqueHelp,
		_elm_lang$core$Basics$identity,
		_elm_lang$core$Set$empty,
		list,
		{ctor: '[]'});
};
var _elm_community$list_extra$List_Extra$dropWhile = F2(
	function (predicate, list) {
		dropWhile:
		while (true) {
			var _p76 = list;
			if (_p76.ctor === '[]') {
				return {ctor: '[]'};
			} else {
				if (predicate(_p76._0)) {
					var _v75 = predicate,
						_v76 = _p76._1;
					predicate = _v75;
					list = _v76;
					continue dropWhile;
				} else {
					return list;
				}
			}
		}
	});
var _elm_community$list_extra$List_Extra$takeWhile = function (predicate) {
	var takeWhileMemo = F2(
		function (memo, list) {
			takeWhileMemo:
			while (true) {
				var _p77 = list;
				if (_p77.ctor === '[]') {
					return _elm_lang$core$List$reverse(memo);
				} else {
					var _p78 = _p77._0;
					if (predicate(_p78)) {
						var _v78 = {ctor: '::', _0: _p78, _1: memo},
							_v79 = _p77._1;
						memo = _v78;
						list = _v79;
						continue takeWhileMemo;
					} else {
						return _elm_lang$core$List$reverse(memo);
					}
				}
			}
		});
	return takeWhileMemo(
		{ctor: '[]'});
};
var _elm_community$list_extra$List_Extra$span = F2(
	function (p, xs) {
		return {
			ctor: '_Tuple2',
			_0: A2(_elm_community$list_extra$List_Extra$takeWhile, p, xs),
			_1: A2(_elm_community$list_extra$List_Extra$dropWhile, p, xs)
		};
	});
var _elm_community$list_extra$List_Extra$break = function (p) {
	return _elm_community$list_extra$List_Extra$span(
		function (_p79) {
			return !p(_p79);
		});
};
var _elm_community$list_extra$List_Extra$groupWhile = F2(
	function (eq, xs_) {
		var _p80 = xs_;
		if (_p80.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			var _p82 = _p80._0;
			var _p81 = A2(
				_elm_community$list_extra$List_Extra$span,
				eq(_p82),
				_p80._1);
			var ys = _p81._0;
			var zs = _p81._1;
			return {
				ctor: '::',
				_0: {ctor: '::', _0: _p82, _1: ys},
				_1: A2(_elm_community$list_extra$List_Extra$groupWhile, eq, zs)
			};
		}
	});
var _elm_community$list_extra$List_Extra$group = _elm_community$list_extra$List_Extra$groupWhile(
	F2(
		function (x, y) {
			return _elm_lang$core$Native_Utils.eq(x, y);
		}));
var _elm_community$list_extra$List_Extra$minimumBy = F2(
	function (f, ls) {
		var minBy = F2(
			function (x, _p83) {
				var _p84 = _p83;
				var _p85 = _p84._1;
				var fx = f(x);
				return (_elm_lang$core$Native_Utils.cmp(fx, _p85) < 0) ? {ctor: '_Tuple2', _0: x, _1: fx} : {ctor: '_Tuple2', _0: _p84._0, _1: _p85};
			});
		var _p86 = ls;
		if (_p86.ctor === '::') {
			if (_p86._1.ctor === '[]') {
				return _elm_lang$core$Maybe$Just(_p86._0);
			} else {
				var _p87 = _p86._0;
				return _elm_lang$core$Maybe$Just(
					_elm_lang$core$Tuple$first(
						A3(
							_elm_lang$core$List$foldl,
							minBy,
							{
								ctor: '_Tuple2',
								_0: _p87,
								_1: f(_p87)
							},
							_p86._1)));
			}
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_community$list_extra$List_Extra$maximumBy = F2(
	function (f, ls) {
		var maxBy = F2(
			function (x, _p88) {
				var _p89 = _p88;
				var _p90 = _p89._1;
				var fx = f(x);
				return (_elm_lang$core$Native_Utils.cmp(fx, _p90) > 0) ? {ctor: '_Tuple2', _0: x, _1: fx} : {ctor: '_Tuple2', _0: _p89._0, _1: _p90};
			});
		var _p91 = ls;
		if (_p91.ctor === '::') {
			if (_p91._1.ctor === '[]') {
				return _elm_lang$core$Maybe$Just(_p91._0);
			} else {
				var _p92 = _p91._0;
				return _elm_lang$core$Maybe$Just(
					_elm_lang$core$Tuple$first(
						A3(
							_elm_lang$core$List$foldl,
							maxBy,
							{
								ctor: '_Tuple2',
								_0: _p92,
								_1: f(_p92)
							},
							_p91._1)));
			}
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_community$list_extra$List_Extra$uncons = function (xs) {
	var _p93 = xs;
	if (_p93.ctor === '[]') {
		return _elm_lang$core$Maybe$Nothing;
	} else {
		return _elm_lang$core$Maybe$Just(
			{ctor: '_Tuple2', _0: _p93._0, _1: _p93._1});
	}
};
var _elm_community$list_extra$List_Extra$swapAt = F3(
	function (index1, index2, l) {
		swapAt:
		while (true) {
			if (_elm_lang$core$Native_Utils.eq(index1, index2) || (_elm_lang$core$Native_Utils.cmp(index1, 0) < 0)) {
				return l;
			} else {
				if (_elm_lang$core$Native_Utils.cmp(index1, index2) > 0) {
					var _v86 = index2,
						_v87 = index1,
						_v88 = l;
					index1 = _v86;
					index2 = _v87;
					l = _v88;
					continue swapAt;
				} else {
					var _p94 = A2(_elm_community$list_extra$List_Extra$splitAt, index1, l);
					var part1 = _p94._0;
					var tail1 = _p94._1;
					var _p95 = A2(_elm_community$list_extra$List_Extra$splitAt, index2 - index1, tail1);
					var head2 = _p95._0;
					var tail2 = _p95._1;
					var _p96 = {
						ctor: '_Tuple2',
						_0: _elm_community$list_extra$List_Extra$uncons(head2),
						_1: _elm_community$list_extra$List_Extra$uncons(tail2)
					};
					if (((((_p96.ctor === '_Tuple2') && (_p96._0.ctor === 'Just')) && (_p96._0._0.ctor === '_Tuple2')) && (_p96._1.ctor === 'Just')) && (_p96._1._0.ctor === '_Tuple2')) {
						return _elm_lang$core$List$concat(
							{
								ctor: '::',
								_0: part1,
								_1: {
									ctor: '::',
									_0: {ctor: '::', _0: _p96._1._0._0, _1: _p96._0._0._1},
									_1: {
										ctor: '::',
										_0: {ctor: '::', _0: _p96._0._0._0, _1: _p96._1._0._1},
										_1: {ctor: '[]'}
									}
								}
							});
					} else {
						return l;
					}
				}
			}
		}
	});
var _elm_community$list_extra$List_Extra$cycleHelp = F3(
	function (acc, n, list) {
		cycleHelp:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(n, 0) > 0) {
				var _v90 = A2(_elm_community$list_extra$List_Extra$reverseAppend, list, acc),
					_v91 = n - 1,
					_v92 = list;
				acc = _v90;
				n = _v91;
				list = _v92;
				continue cycleHelp;
			} else {
				return acc;
			}
		}
	});
var _elm_community$list_extra$List_Extra$cycle = F2(
	function (len, list) {
		var cycleLength = _elm_lang$core$List$length(list);
		return (_elm_lang$core$Native_Utils.eq(cycleLength, 0) || _elm_lang$core$Native_Utils.eq(cycleLength, len)) ? list : ((_elm_lang$core$Native_Utils.cmp(cycleLength, len) < 0) ? _elm_lang$core$List$reverse(
			A2(
				_elm_community$list_extra$List_Extra$reverseAppend,
				A2(
					_elm_lang$core$List$take,
					A2(_elm_lang$core$Basics$rem, len, cycleLength),
					list),
				A3(
					_elm_community$list_extra$List_Extra$cycleHelp,
					{ctor: '[]'},
					(len / cycleLength) | 0,
					list))) : A2(_elm_lang$core$List$take, len, list));
	});
var _elm_community$list_extra$List_Extra$initialize = F2(
	function (n, f) {
		var step = F2(
			function (i, acc) {
				step:
				while (true) {
					if (_elm_lang$core$Native_Utils.cmp(i, 0) < 0) {
						return acc;
					} else {
						var _v93 = i - 1,
							_v94 = {
							ctor: '::',
							_0: f(i),
							_1: acc
						};
						i = _v93;
						acc = _v94;
						continue step;
					}
				}
			});
		return A2(
			step,
			n - 1,
			{ctor: '[]'});
	});
var _elm_community$list_extra$List_Extra$iterate = F2(
	function (f, x) {
		var _p97 = f(x);
		if (_p97.ctor === 'Just') {
			return {
				ctor: '::',
				_0: x,
				_1: A2(_elm_community$list_extra$List_Extra$iterate, f, _p97._0)
			};
		} else {
			return {
				ctor: '::',
				_0: x,
				_1: {ctor: '[]'}
			};
		}
	});
var _elm_community$list_extra$List_Extra$getAt = F2(
	function (idx, xs) {
		return (_elm_lang$core$Native_Utils.cmp(idx, 0) < 0) ? _elm_lang$core$Maybe$Nothing : _elm_lang$core$List$head(
			A2(_elm_lang$core$List$drop, idx, xs));
	});
var _elm_community$list_extra$List_Extra_ops = _elm_community$list_extra$List_Extra_ops || {};
_elm_community$list_extra$List_Extra_ops['!!'] = _elm_lang$core$Basics$flip(_elm_community$list_extra$List_Extra$getAt);
var _elm_community$list_extra$List_Extra$init = function (items) {
	var _p98 = items;
	if (_p98.ctor === '[]') {
		return _elm_lang$core$Maybe$Nothing;
	} else {
		return A2(
			_elm_lang$core$Maybe$map,
			_elm_lang$core$List$reverse,
			_elm_lang$core$List$tail(
				_elm_lang$core$List$reverse(_p98)));
	}
};
var _elm_community$list_extra$List_Extra$last = function (items) {
	last:
	while (true) {
		var _p99 = items;
		if (_p99.ctor === '[]') {
			return _elm_lang$core$Maybe$Nothing;
		} else {
			if (_p99._1.ctor === '[]') {
				return _elm_lang$core$Maybe$Just(_p99._0);
			} else {
				var _v98 = _p99._1;
				items = _v98;
				continue last;
			}
		}
	}
};

var _elm_lang$http$Native_Http = function() {


// ENCODING AND DECODING

function encodeUri(string)
{
	return encodeURIComponent(string);
}

function decodeUri(string)
{
	try
	{
		return _elm_lang$core$Maybe$Just(decodeURIComponent(string));
	}
	catch(e)
	{
		return _elm_lang$core$Maybe$Nothing;
	}
}


// SEND REQUEST

function toTask(request, maybeProgress)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		var xhr = new XMLHttpRequest();

		configureProgress(xhr, maybeProgress);

		xhr.addEventListener('error', function() {
			callback(_elm_lang$core$Native_Scheduler.fail({ ctor: 'NetworkError' }));
		});
		xhr.addEventListener('timeout', function() {
			callback(_elm_lang$core$Native_Scheduler.fail({ ctor: 'Timeout' }));
		});
		xhr.addEventListener('load', function() {
			callback(handleResponse(xhr, request.expect.responseToResult));
		});

		try
		{
			xhr.open(request.method, request.url, true);
		}
		catch (e)
		{
			return callback(_elm_lang$core$Native_Scheduler.fail({ ctor: 'BadUrl', _0: request.url }));
		}

		configureRequest(xhr, request);
		send(xhr, request.body);

		return function() { xhr.abort(); };
	});
}

function configureProgress(xhr, maybeProgress)
{
	if (maybeProgress.ctor === 'Nothing')
	{
		return;
	}

	xhr.addEventListener('progress', function(event) {
		if (!event.lengthComputable)
		{
			return;
		}
		_elm_lang$core$Native_Scheduler.rawSpawn(maybeProgress._0({
			bytes: event.loaded,
			bytesExpected: event.total
		}));
	});
}

function configureRequest(xhr, request)
{
	function setHeader(pair)
	{
		xhr.setRequestHeader(pair._0, pair._1);
	}

	A2(_elm_lang$core$List$map, setHeader, request.headers);
	xhr.responseType = request.expect.responseType;
	xhr.withCredentials = request.withCredentials;

	if (request.timeout.ctor === 'Just')
	{
		xhr.timeout = request.timeout._0;
	}
}

function send(xhr, body)
{
	switch (body.ctor)
	{
		case 'EmptyBody':
			xhr.send();
			return;

		case 'StringBody':
			xhr.setRequestHeader('Content-Type', body._0);
			xhr.send(body._1);
			return;

		case 'FormDataBody':
			xhr.send(body._0);
			return;
	}
}


// RESPONSES

function handleResponse(xhr, responseToResult)
{
	var response = toResponse(xhr);

	if (xhr.status < 200 || 300 <= xhr.status)
	{
		response.body = xhr.responseText;
		return _elm_lang$core$Native_Scheduler.fail({
			ctor: 'BadStatus',
			_0: response
		});
	}

	var result = responseToResult(response);

	if (result.ctor === 'Ok')
	{
		return _elm_lang$core$Native_Scheduler.succeed(result._0);
	}
	else
	{
		response.body = xhr.responseText;
		return _elm_lang$core$Native_Scheduler.fail({
			ctor: 'BadPayload',
			_0: result._0,
			_1: response
		});
	}
}

function toResponse(xhr)
{
	return {
		status: { code: xhr.status, message: xhr.statusText },
		headers: parseHeaders(xhr.getAllResponseHeaders()),
		url: xhr.responseURL,
		body: xhr.response
	};
}

function parseHeaders(rawHeaders)
{
	var headers = _elm_lang$core$Dict$empty;

	if (!rawHeaders)
	{
		return headers;
	}

	var headerPairs = rawHeaders.split('\u000d\u000a');
	for (var i = headerPairs.length; i--; )
	{
		var headerPair = headerPairs[i];
		var index = headerPair.indexOf('\u003a\u0020');
		if (index > 0)
		{
			var key = headerPair.substring(0, index);
			var value = headerPair.substring(index + 2);

			headers = A3(_elm_lang$core$Dict$update, key, function(oldValue) {
				if (oldValue.ctor === 'Just')
				{
					return _elm_lang$core$Maybe$Just(value + ', ' + oldValue._0);
				}
				return _elm_lang$core$Maybe$Just(value);
			}, headers);
		}
	}

	return headers;
}


// EXPECTORS

function expectStringResponse(responseToResult)
{
	return {
		responseType: 'text',
		responseToResult: responseToResult
	};
}

function mapExpect(func, expect)
{
	return {
		responseType: expect.responseType,
		responseToResult: function(response) {
			var convertedResponse = expect.responseToResult(response);
			return A2(_elm_lang$core$Result$map, func, convertedResponse);
		}
	};
}


// BODY

function multipart(parts)
{
	var formData = new FormData();

	while (parts.ctor !== '[]')
	{
		var part = parts._0;
		formData.append(part._0, part._1);
		parts = parts._1;
	}

	return { ctor: 'FormDataBody', _0: formData };
}

return {
	toTask: F2(toTask),
	expectStringResponse: expectStringResponse,
	mapExpect: F2(mapExpect),
	multipart: multipart,
	encodeUri: encodeUri,
	decodeUri: decodeUri
};

}();

var _elm_lang$http$Http_Internal$map = F2(
	function (func, request) {
		return _elm_lang$core$Native_Utils.update(
			request,
			{
				expect: A2(_elm_lang$http$Native_Http.mapExpect, func, request.expect)
			});
	});
var _elm_lang$http$Http_Internal$RawRequest = F7(
	function (a, b, c, d, e, f, g) {
		return {method: a, headers: b, url: c, body: d, expect: e, timeout: f, withCredentials: g};
	});
var _elm_lang$http$Http_Internal$Request = function (a) {
	return {ctor: 'Request', _0: a};
};
var _elm_lang$http$Http_Internal$Expect = {ctor: 'Expect'};
var _elm_lang$http$Http_Internal$FormDataBody = {ctor: 'FormDataBody'};
var _elm_lang$http$Http_Internal$StringBody = F2(
	function (a, b) {
		return {ctor: 'StringBody', _0: a, _1: b};
	});
var _elm_lang$http$Http_Internal$EmptyBody = {ctor: 'EmptyBody'};
var _elm_lang$http$Http_Internal$Header = F2(
	function (a, b) {
		return {ctor: 'Header', _0: a, _1: b};
	});

var _elm_lang$http$Http$decodeUri = _elm_lang$http$Native_Http.decodeUri;
var _elm_lang$http$Http$encodeUri = _elm_lang$http$Native_Http.encodeUri;
var _elm_lang$http$Http$expectStringResponse = _elm_lang$http$Native_Http.expectStringResponse;
var _elm_lang$http$Http$expectJson = function (decoder) {
	return _elm_lang$http$Http$expectStringResponse(
		function (response) {
			return A2(_elm_lang$core$Json_Decode$decodeString, decoder, response.body);
		});
};
var _elm_lang$http$Http$expectString = _elm_lang$http$Http$expectStringResponse(
	function (response) {
		return _elm_lang$core$Result$Ok(response.body);
	});
var _elm_lang$http$Http$multipartBody = _elm_lang$http$Native_Http.multipart;
var _elm_lang$http$Http$stringBody = _elm_lang$http$Http_Internal$StringBody;
var _elm_lang$http$Http$jsonBody = function (value) {
	return A2(
		_elm_lang$http$Http_Internal$StringBody,
		'application/json',
		A2(_elm_lang$core$Json_Encode$encode, 0, value));
};
var _elm_lang$http$Http$emptyBody = _elm_lang$http$Http_Internal$EmptyBody;
var _elm_lang$http$Http$header = _elm_lang$http$Http_Internal$Header;
var _elm_lang$http$Http$request = _elm_lang$http$Http_Internal$Request;
var _elm_lang$http$Http$post = F3(
	function (url, body, decoder) {
		return _elm_lang$http$Http$request(
			{
				method: 'POST',
				headers: {ctor: '[]'},
				url: url,
				body: body,
				expect: _elm_lang$http$Http$expectJson(decoder),
				timeout: _elm_lang$core$Maybe$Nothing,
				withCredentials: false
			});
	});
var _elm_lang$http$Http$get = F2(
	function (url, decoder) {
		return _elm_lang$http$Http$request(
			{
				method: 'GET',
				headers: {ctor: '[]'},
				url: url,
				body: _elm_lang$http$Http$emptyBody,
				expect: _elm_lang$http$Http$expectJson(decoder),
				timeout: _elm_lang$core$Maybe$Nothing,
				withCredentials: false
			});
	});
var _elm_lang$http$Http$getString = function (url) {
	return _elm_lang$http$Http$request(
		{
			method: 'GET',
			headers: {ctor: '[]'},
			url: url,
			body: _elm_lang$http$Http$emptyBody,
			expect: _elm_lang$http$Http$expectString,
			timeout: _elm_lang$core$Maybe$Nothing,
			withCredentials: false
		});
};
var _elm_lang$http$Http$toTask = function (_p0) {
	var _p1 = _p0;
	return A2(_elm_lang$http$Native_Http.toTask, _p1._0, _elm_lang$core$Maybe$Nothing);
};
var _elm_lang$http$Http$send = F2(
	function (resultToMessage, request) {
		return A2(
			_elm_lang$core$Task$attempt,
			resultToMessage,
			_elm_lang$http$Http$toTask(request));
	});
var _elm_lang$http$Http$Response = F4(
	function (a, b, c, d) {
		return {url: a, status: b, headers: c, body: d};
	});
var _elm_lang$http$Http$BadPayload = F2(
	function (a, b) {
		return {ctor: 'BadPayload', _0: a, _1: b};
	});
var _elm_lang$http$Http$BadStatus = function (a) {
	return {ctor: 'BadStatus', _0: a};
};
var _elm_lang$http$Http$NetworkError = {ctor: 'NetworkError'};
var _elm_lang$http$Http$Timeout = {ctor: 'Timeout'};
var _elm_lang$http$Http$BadUrl = function (a) {
	return {ctor: 'BadUrl', _0: a};
};
var _elm_lang$http$Http$StringPart = F2(
	function (a, b) {
		return {ctor: 'StringPart', _0: a, _1: b};
	});
var _elm_lang$http$Http$stringPart = _elm_lang$http$Http$StringPart;

var _elm_lang$svg$Svg$map = _elm_lang$virtual_dom$VirtualDom$map;
var _elm_lang$svg$Svg$text = _elm_lang$virtual_dom$VirtualDom$text;
var _elm_lang$svg$Svg$svgNamespace = A2(
	_elm_lang$virtual_dom$VirtualDom$property,
	'namespace',
	_elm_lang$core$Json_Encode$string('http://www.w3.org/2000/svg'));
var _elm_lang$svg$Svg$node = F3(
	function (name, attributes, children) {
		return A3(
			_elm_lang$virtual_dom$VirtualDom$node,
			name,
			{ctor: '::', _0: _elm_lang$svg$Svg$svgNamespace, _1: attributes},
			children);
	});
var _elm_lang$svg$Svg$svg = _elm_lang$svg$Svg$node('svg');
var _elm_lang$svg$Svg$foreignObject = _elm_lang$svg$Svg$node('foreignObject');
var _elm_lang$svg$Svg$animate = _elm_lang$svg$Svg$node('animate');
var _elm_lang$svg$Svg$animateColor = _elm_lang$svg$Svg$node('animateColor');
var _elm_lang$svg$Svg$animateMotion = _elm_lang$svg$Svg$node('animateMotion');
var _elm_lang$svg$Svg$animateTransform = _elm_lang$svg$Svg$node('animateTransform');
var _elm_lang$svg$Svg$mpath = _elm_lang$svg$Svg$node('mpath');
var _elm_lang$svg$Svg$set = _elm_lang$svg$Svg$node('set');
var _elm_lang$svg$Svg$a = _elm_lang$svg$Svg$node('a');
var _elm_lang$svg$Svg$defs = _elm_lang$svg$Svg$node('defs');
var _elm_lang$svg$Svg$g = _elm_lang$svg$Svg$node('g');
var _elm_lang$svg$Svg$marker = _elm_lang$svg$Svg$node('marker');
var _elm_lang$svg$Svg$mask = _elm_lang$svg$Svg$node('mask');
var _elm_lang$svg$Svg$pattern = _elm_lang$svg$Svg$node('pattern');
var _elm_lang$svg$Svg$switch = _elm_lang$svg$Svg$node('switch');
var _elm_lang$svg$Svg$symbol = _elm_lang$svg$Svg$node('symbol');
var _elm_lang$svg$Svg$desc = _elm_lang$svg$Svg$node('desc');
var _elm_lang$svg$Svg$metadata = _elm_lang$svg$Svg$node('metadata');
var _elm_lang$svg$Svg$title = _elm_lang$svg$Svg$node('title');
var _elm_lang$svg$Svg$feBlend = _elm_lang$svg$Svg$node('feBlend');
var _elm_lang$svg$Svg$feColorMatrix = _elm_lang$svg$Svg$node('feColorMatrix');
var _elm_lang$svg$Svg$feComponentTransfer = _elm_lang$svg$Svg$node('feComponentTransfer');
var _elm_lang$svg$Svg$feComposite = _elm_lang$svg$Svg$node('feComposite');
var _elm_lang$svg$Svg$feConvolveMatrix = _elm_lang$svg$Svg$node('feConvolveMatrix');
var _elm_lang$svg$Svg$feDiffuseLighting = _elm_lang$svg$Svg$node('feDiffuseLighting');
var _elm_lang$svg$Svg$feDisplacementMap = _elm_lang$svg$Svg$node('feDisplacementMap');
var _elm_lang$svg$Svg$feFlood = _elm_lang$svg$Svg$node('feFlood');
var _elm_lang$svg$Svg$feFuncA = _elm_lang$svg$Svg$node('feFuncA');
var _elm_lang$svg$Svg$feFuncB = _elm_lang$svg$Svg$node('feFuncB');
var _elm_lang$svg$Svg$feFuncG = _elm_lang$svg$Svg$node('feFuncG');
var _elm_lang$svg$Svg$feFuncR = _elm_lang$svg$Svg$node('feFuncR');
var _elm_lang$svg$Svg$feGaussianBlur = _elm_lang$svg$Svg$node('feGaussianBlur');
var _elm_lang$svg$Svg$feImage = _elm_lang$svg$Svg$node('feImage');
var _elm_lang$svg$Svg$feMerge = _elm_lang$svg$Svg$node('feMerge');
var _elm_lang$svg$Svg$feMergeNode = _elm_lang$svg$Svg$node('feMergeNode');
var _elm_lang$svg$Svg$feMorphology = _elm_lang$svg$Svg$node('feMorphology');
var _elm_lang$svg$Svg$feOffset = _elm_lang$svg$Svg$node('feOffset');
var _elm_lang$svg$Svg$feSpecularLighting = _elm_lang$svg$Svg$node('feSpecularLighting');
var _elm_lang$svg$Svg$feTile = _elm_lang$svg$Svg$node('feTile');
var _elm_lang$svg$Svg$feTurbulence = _elm_lang$svg$Svg$node('feTurbulence');
var _elm_lang$svg$Svg$font = _elm_lang$svg$Svg$node('font');
var _elm_lang$svg$Svg$linearGradient = _elm_lang$svg$Svg$node('linearGradient');
var _elm_lang$svg$Svg$radialGradient = _elm_lang$svg$Svg$node('radialGradient');
var _elm_lang$svg$Svg$stop = _elm_lang$svg$Svg$node('stop');
var _elm_lang$svg$Svg$circle = _elm_lang$svg$Svg$node('circle');
var _elm_lang$svg$Svg$ellipse = _elm_lang$svg$Svg$node('ellipse');
var _elm_lang$svg$Svg$image = _elm_lang$svg$Svg$node('image');
var _elm_lang$svg$Svg$line = _elm_lang$svg$Svg$node('line');
var _elm_lang$svg$Svg$path = _elm_lang$svg$Svg$node('path');
var _elm_lang$svg$Svg$polygon = _elm_lang$svg$Svg$node('polygon');
var _elm_lang$svg$Svg$polyline = _elm_lang$svg$Svg$node('polyline');
var _elm_lang$svg$Svg$rect = _elm_lang$svg$Svg$node('rect');
var _elm_lang$svg$Svg$use = _elm_lang$svg$Svg$node('use');
var _elm_lang$svg$Svg$feDistantLight = _elm_lang$svg$Svg$node('feDistantLight');
var _elm_lang$svg$Svg$fePointLight = _elm_lang$svg$Svg$node('fePointLight');
var _elm_lang$svg$Svg$feSpotLight = _elm_lang$svg$Svg$node('feSpotLight');
var _elm_lang$svg$Svg$altGlyph = _elm_lang$svg$Svg$node('altGlyph');
var _elm_lang$svg$Svg$altGlyphDef = _elm_lang$svg$Svg$node('altGlyphDef');
var _elm_lang$svg$Svg$altGlyphItem = _elm_lang$svg$Svg$node('altGlyphItem');
var _elm_lang$svg$Svg$glyph = _elm_lang$svg$Svg$node('glyph');
var _elm_lang$svg$Svg$glyphRef = _elm_lang$svg$Svg$node('glyphRef');
var _elm_lang$svg$Svg$textPath = _elm_lang$svg$Svg$node('textPath');
var _elm_lang$svg$Svg$text_ = _elm_lang$svg$Svg$node('text');
var _elm_lang$svg$Svg$tref = _elm_lang$svg$Svg$node('tref');
var _elm_lang$svg$Svg$tspan = _elm_lang$svg$Svg$node('tspan');
var _elm_lang$svg$Svg$clipPath = _elm_lang$svg$Svg$node('clipPath');
var _elm_lang$svg$Svg$colorProfile = _elm_lang$svg$Svg$node('colorProfile');
var _elm_lang$svg$Svg$cursor = _elm_lang$svg$Svg$node('cursor');
var _elm_lang$svg$Svg$filter = _elm_lang$svg$Svg$node('filter');
var _elm_lang$svg$Svg$script = _elm_lang$svg$Svg$node('script');
var _elm_lang$svg$Svg$style = _elm_lang$svg$Svg$node('style');
var _elm_lang$svg$Svg$view = _elm_lang$svg$Svg$node('view');

var _elm_lang$svg$Svg_Attributes$writingMode = _elm_lang$virtual_dom$VirtualDom$attribute('writing-mode');
var _elm_lang$svg$Svg_Attributes$wordSpacing = _elm_lang$virtual_dom$VirtualDom$attribute('word-spacing');
var _elm_lang$svg$Svg_Attributes$visibility = _elm_lang$virtual_dom$VirtualDom$attribute('visibility');
var _elm_lang$svg$Svg_Attributes$unicodeBidi = _elm_lang$virtual_dom$VirtualDom$attribute('unicode-bidi');
var _elm_lang$svg$Svg_Attributes$textRendering = _elm_lang$virtual_dom$VirtualDom$attribute('text-rendering');
var _elm_lang$svg$Svg_Attributes$textDecoration = _elm_lang$virtual_dom$VirtualDom$attribute('text-decoration');
var _elm_lang$svg$Svg_Attributes$textAnchor = _elm_lang$virtual_dom$VirtualDom$attribute('text-anchor');
var _elm_lang$svg$Svg_Attributes$stroke = _elm_lang$virtual_dom$VirtualDom$attribute('stroke');
var _elm_lang$svg$Svg_Attributes$strokeWidth = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-width');
var _elm_lang$svg$Svg_Attributes$strokeOpacity = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-opacity');
var _elm_lang$svg$Svg_Attributes$strokeMiterlimit = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-miterlimit');
var _elm_lang$svg$Svg_Attributes$strokeLinejoin = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-linejoin');
var _elm_lang$svg$Svg_Attributes$strokeLinecap = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-linecap');
var _elm_lang$svg$Svg_Attributes$strokeDashoffset = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-dashoffset');
var _elm_lang$svg$Svg_Attributes$strokeDasharray = _elm_lang$virtual_dom$VirtualDom$attribute('stroke-dasharray');
var _elm_lang$svg$Svg_Attributes$stopOpacity = _elm_lang$virtual_dom$VirtualDom$attribute('stop-opacity');
var _elm_lang$svg$Svg_Attributes$stopColor = _elm_lang$virtual_dom$VirtualDom$attribute('stop-color');
var _elm_lang$svg$Svg_Attributes$shapeRendering = _elm_lang$virtual_dom$VirtualDom$attribute('shape-rendering');
var _elm_lang$svg$Svg_Attributes$pointerEvents = _elm_lang$virtual_dom$VirtualDom$attribute('pointer-events');
var _elm_lang$svg$Svg_Attributes$overflow = _elm_lang$virtual_dom$VirtualDom$attribute('overflow');
var _elm_lang$svg$Svg_Attributes$opacity = _elm_lang$virtual_dom$VirtualDom$attribute('opacity');
var _elm_lang$svg$Svg_Attributes$mask = _elm_lang$virtual_dom$VirtualDom$attribute('mask');
var _elm_lang$svg$Svg_Attributes$markerStart = _elm_lang$virtual_dom$VirtualDom$attribute('marker-start');
var _elm_lang$svg$Svg_Attributes$markerMid = _elm_lang$virtual_dom$VirtualDom$attribute('marker-mid');
var _elm_lang$svg$Svg_Attributes$markerEnd = _elm_lang$virtual_dom$VirtualDom$attribute('marker-end');
var _elm_lang$svg$Svg_Attributes$lightingColor = _elm_lang$virtual_dom$VirtualDom$attribute('lighting-color');
var _elm_lang$svg$Svg_Attributes$letterSpacing = _elm_lang$virtual_dom$VirtualDom$attribute('letter-spacing');
var _elm_lang$svg$Svg_Attributes$kerning = _elm_lang$virtual_dom$VirtualDom$attribute('kerning');
var _elm_lang$svg$Svg_Attributes$imageRendering = _elm_lang$virtual_dom$VirtualDom$attribute('image-rendering');
var _elm_lang$svg$Svg_Attributes$glyphOrientationVertical = _elm_lang$virtual_dom$VirtualDom$attribute('glyph-orientation-vertical');
var _elm_lang$svg$Svg_Attributes$glyphOrientationHorizontal = _elm_lang$virtual_dom$VirtualDom$attribute('glyph-orientation-horizontal');
var _elm_lang$svg$Svg_Attributes$fontWeight = _elm_lang$virtual_dom$VirtualDom$attribute('font-weight');
var _elm_lang$svg$Svg_Attributes$fontVariant = _elm_lang$virtual_dom$VirtualDom$attribute('font-variant');
var _elm_lang$svg$Svg_Attributes$fontStyle = _elm_lang$virtual_dom$VirtualDom$attribute('font-style');
var _elm_lang$svg$Svg_Attributes$fontStretch = _elm_lang$virtual_dom$VirtualDom$attribute('font-stretch');
var _elm_lang$svg$Svg_Attributes$fontSize = _elm_lang$virtual_dom$VirtualDom$attribute('font-size');
var _elm_lang$svg$Svg_Attributes$fontSizeAdjust = _elm_lang$virtual_dom$VirtualDom$attribute('font-size-adjust');
var _elm_lang$svg$Svg_Attributes$fontFamily = _elm_lang$virtual_dom$VirtualDom$attribute('font-family');
var _elm_lang$svg$Svg_Attributes$floodOpacity = _elm_lang$virtual_dom$VirtualDom$attribute('flood-opacity');
var _elm_lang$svg$Svg_Attributes$floodColor = _elm_lang$virtual_dom$VirtualDom$attribute('flood-color');
var _elm_lang$svg$Svg_Attributes$filter = _elm_lang$virtual_dom$VirtualDom$attribute('filter');
var _elm_lang$svg$Svg_Attributes$fill = _elm_lang$virtual_dom$VirtualDom$attribute('fill');
var _elm_lang$svg$Svg_Attributes$fillRule = _elm_lang$virtual_dom$VirtualDom$attribute('fill-rule');
var _elm_lang$svg$Svg_Attributes$fillOpacity = _elm_lang$virtual_dom$VirtualDom$attribute('fill-opacity');
var _elm_lang$svg$Svg_Attributes$enableBackground = _elm_lang$virtual_dom$VirtualDom$attribute('enable-background');
var _elm_lang$svg$Svg_Attributes$dominantBaseline = _elm_lang$virtual_dom$VirtualDom$attribute('dominant-baseline');
var _elm_lang$svg$Svg_Attributes$display = _elm_lang$virtual_dom$VirtualDom$attribute('display');
var _elm_lang$svg$Svg_Attributes$direction = _elm_lang$virtual_dom$VirtualDom$attribute('direction');
var _elm_lang$svg$Svg_Attributes$cursor = _elm_lang$virtual_dom$VirtualDom$attribute('cursor');
var _elm_lang$svg$Svg_Attributes$color = _elm_lang$virtual_dom$VirtualDom$attribute('color');
var _elm_lang$svg$Svg_Attributes$colorRendering = _elm_lang$virtual_dom$VirtualDom$attribute('color-rendering');
var _elm_lang$svg$Svg_Attributes$colorProfile = _elm_lang$virtual_dom$VirtualDom$attribute('color-profile');
var _elm_lang$svg$Svg_Attributes$colorInterpolation = _elm_lang$virtual_dom$VirtualDom$attribute('color-interpolation');
var _elm_lang$svg$Svg_Attributes$colorInterpolationFilters = _elm_lang$virtual_dom$VirtualDom$attribute('color-interpolation-filters');
var _elm_lang$svg$Svg_Attributes$clip = _elm_lang$virtual_dom$VirtualDom$attribute('clip');
var _elm_lang$svg$Svg_Attributes$clipRule = _elm_lang$virtual_dom$VirtualDom$attribute('clip-rule');
var _elm_lang$svg$Svg_Attributes$clipPath = _elm_lang$virtual_dom$VirtualDom$attribute('clip-path');
var _elm_lang$svg$Svg_Attributes$baselineShift = _elm_lang$virtual_dom$VirtualDom$attribute('baseline-shift');
var _elm_lang$svg$Svg_Attributes$alignmentBaseline = _elm_lang$virtual_dom$VirtualDom$attribute('alignment-baseline');
var _elm_lang$svg$Svg_Attributes$zoomAndPan = _elm_lang$virtual_dom$VirtualDom$attribute('zoomAndPan');
var _elm_lang$svg$Svg_Attributes$z = _elm_lang$virtual_dom$VirtualDom$attribute('z');
var _elm_lang$svg$Svg_Attributes$yChannelSelector = _elm_lang$virtual_dom$VirtualDom$attribute('yChannelSelector');
var _elm_lang$svg$Svg_Attributes$y2 = _elm_lang$virtual_dom$VirtualDom$attribute('y2');
var _elm_lang$svg$Svg_Attributes$y1 = _elm_lang$virtual_dom$VirtualDom$attribute('y1');
var _elm_lang$svg$Svg_Attributes$y = _elm_lang$virtual_dom$VirtualDom$attribute('y');
var _elm_lang$svg$Svg_Attributes$xmlSpace = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/XML/1998/namespace', 'xml:space');
var _elm_lang$svg$Svg_Attributes$xmlLang = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/XML/1998/namespace', 'xml:lang');
var _elm_lang$svg$Svg_Attributes$xmlBase = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/XML/1998/namespace', 'xml:base');
var _elm_lang$svg$Svg_Attributes$xlinkType = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:type');
var _elm_lang$svg$Svg_Attributes$xlinkTitle = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:title');
var _elm_lang$svg$Svg_Attributes$xlinkShow = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:show');
var _elm_lang$svg$Svg_Attributes$xlinkRole = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:role');
var _elm_lang$svg$Svg_Attributes$xlinkHref = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:href');
var _elm_lang$svg$Svg_Attributes$xlinkArcrole = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:arcrole');
var _elm_lang$svg$Svg_Attributes$xlinkActuate = A2(_elm_lang$virtual_dom$VirtualDom$attributeNS, 'http://www.w3.org/1999/xlink', 'xlink:actuate');
var _elm_lang$svg$Svg_Attributes$xChannelSelector = _elm_lang$virtual_dom$VirtualDom$attribute('xChannelSelector');
var _elm_lang$svg$Svg_Attributes$x2 = _elm_lang$virtual_dom$VirtualDom$attribute('x2');
var _elm_lang$svg$Svg_Attributes$x1 = _elm_lang$virtual_dom$VirtualDom$attribute('x1');
var _elm_lang$svg$Svg_Attributes$xHeight = _elm_lang$virtual_dom$VirtualDom$attribute('x-height');
var _elm_lang$svg$Svg_Attributes$x = _elm_lang$virtual_dom$VirtualDom$attribute('x');
var _elm_lang$svg$Svg_Attributes$widths = _elm_lang$virtual_dom$VirtualDom$attribute('widths');
var _elm_lang$svg$Svg_Attributes$width = _elm_lang$virtual_dom$VirtualDom$attribute('width');
var _elm_lang$svg$Svg_Attributes$viewTarget = _elm_lang$virtual_dom$VirtualDom$attribute('viewTarget');
var _elm_lang$svg$Svg_Attributes$viewBox = _elm_lang$virtual_dom$VirtualDom$attribute('viewBox');
var _elm_lang$svg$Svg_Attributes$vertOriginY = _elm_lang$virtual_dom$VirtualDom$attribute('vert-origin-y');
var _elm_lang$svg$Svg_Attributes$vertOriginX = _elm_lang$virtual_dom$VirtualDom$attribute('vert-origin-x');
var _elm_lang$svg$Svg_Attributes$vertAdvY = _elm_lang$virtual_dom$VirtualDom$attribute('vert-adv-y');
var _elm_lang$svg$Svg_Attributes$version = _elm_lang$virtual_dom$VirtualDom$attribute('version');
var _elm_lang$svg$Svg_Attributes$values = _elm_lang$virtual_dom$VirtualDom$attribute('values');
var _elm_lang$svg$Svg_Attributes$vMathematical = _elm_lang$virtual_dom$VirtualDom$attribute('v-mathematical');
var _elm_lang$svg$Svg_Attributes$vIdeographic = _elm_lang$virtual_dom$VirtualDom$attribute('v-ideographic');
var _elm_lang$svg$Svg_Attributes$vHanging = _elm_lang$virtual_dom$VirtualDom$attribute('v-hanging');
var _elm_lang$svg$Svg_Attributes$vAlphabetic = _elm_lang$virtual_dom$VirtualDom$attribute('v-alphabetic');
var _elm_lang$svg$Svg_Attributes$unitsPerEm = _elm_lang$virtual_dom$VirtualDom$attribute('units-per-em');
var _elm_lang$svg$Svg_Attributes$unicodeRange = _elm_lang$virtual_dom$VirtualDom$attribute('unicode-range');
var _elm_lang$svg$Svg_Attributes$unicode = _elm_lang$virtual_dom$VirtualDom$attribute('unicode');
var _elm_lang$svg$Svg_Attributes$underlineThickness = _elm_lang$virtual_dom$VirtualDom$attribute('underline-thickness');
var _elm_lang$svg$Svg_Attributes$underlinePosition = _elm_lang$virtual_dom$VirtualDom$attribute('underline-position');
var _elm_lang$svg$Svg_Attributes$u2 = _elm_lang$virtual_dom$VirtualDom$attribute('u2');
var _elm_lang$svg$Svg_Attributes$u1 = _elm_lang$virtual_dom$VirtualDom$attribute('u1');
var _elm_lang$svg$Svg_Attributes$type_ = _elm_lang$virtual_dom$VirtualDom$attribute('type');
var _elm_lang$svg$Svg_Attributes$transform = _elm_lang$virtual_dom$VirtualDom$attribute('transform');
var _elm_lang$svg$Svg_Attributes$to = _elm_lang$virtual_dom$VirtualDom$attribute('to');
var _elm_lang$svg$Svg_Attributes$title = _elm_lang$virtual_dom$VirtualDom$attribute('title');
var _elm_lang$svg$Svg_Attributes$textLength = _elm_lang$virtual_dom$VirtualDom$attribute('textLength');
var _elm_lang$svg$Svg_Attributes$targetY = _elm_lang$virtual_dom$VirtualDom$attribute('targetY');
var _elm_lang$svg$Svg_Attributes$targetX = _elm_lang$virtual_dom$VirtualDom$attribute('targetX');
var _elm_lang$svg$Svg_Attributes$target = _elm_lang$virtual_dom$VirtualDom$attribute('target');
var _elm_lang$svg$Svg_Attributes$tableValues = _elm_lang$virtual_dom$VirtualDom$attribute('tableValues');
var _elm_lang$svg$Svg_Attributes$systemLanguage = _elm_lang$virtual_dom$VirtualDom$attribute('systemLanguage');
var _elm_lang$svg$Svg_Attributes$surfaceScale = _elm_lang$virtual_dom$VirtualDom$attribute('surfaceScale');
var _elm_lang$svg$Svg_Attributes$style = _elm_lang$virtual_dom$VirtualDom$attribute('style');
var _elm_lang$svg$Svg_Attributes$string = _elm_lang$virtual_dom$VirtualDom$attribute('string');
var _elm_lang$svg$Svg_Attributes$strikethroughThickness = _elm_lang$virtual_dom$VirtualDom$attribute('strikethrough-thickness');
var _elm_lang$svg$Svg_Attributes$strikethroughPosition = _elm_lang$virtual_dom$VirtualDom$attribute('strikethrough-position');
var _elm_lang$svg$Svg_Attributes$stitchTiles = _elm_lang$virtual_dom$VirtualDom$attribute('stitchTiles');
var _elm_lang$svg$Svg_Attributes$stemv = _elm_lang$virtual_dom$VirtualDom$attribute('stemv');
var _elm_lang$svg$Svg_Attributes$stemh = _elm_lang$virtual_dom$VirtualDom$attribute('stemh');
var _elm_lang$svg$Svg_Attributes$stdDeviation = _elm_lang$virtual_dom$VirtualDom$attribute('stdDeviation');
var _elm_lang$svg$Svg_Attributes$startOffset = _elm_lang$virtual_dom$VirtualDom$attribute('startOffset');
var _elm_lang$svg$Svg_Attributes$spreadMethod = _elm_lang$virtual_dom$VirtualDom$attribute('spreadMethod');
var _elm_lang$svg$Svg_Attributes$speed = _elm_lang$virtual_dom$VirtualDom$attribute('speed');
var _elm_lang$svg$Svg_Attributes$specularExponent = _elm_lang$virtual_dom$VirtualDom$attribute('specularExponent');
var _elm_lang$svg$Svg_Attributes$specularConstant = _elm_lang$virtual_dom$VirtualDom$attribute('specularConstant');
var _elm_lang$svg$Svg_Attributes$spacing = _elm_lang$virtual_dom$VirtualDom$attribute('spacing');
var _elm_lang$svg$Svg_Attributes$slope = _elm_lang$virtual_dom$VirtualDom$attribute('slope');
var _elm_lang$svg$Svg_Attributes$seed = _elm_lang$virtual_dom$VirtualDom$attribute('seed');
var _elm_lang$svg$Svg_Attributes$scale = _elm_lang$virtual_dom$VirtualDom$attribute('scale');
var _elm_lang$svg$Svg_Attributes$ry = _elm_lang$virtual_dom$VirtualDom$attribute('ry');
var _elm_lang$svg$Svg_Attributes$rx = _elm_lang$virtual_dom$VirtualDom$attribute('rx');
var _elm_lang$svg$Svg_Attributes$rotate = _elm_lang$virtual_dom$VirtualDom$attribute('rotate');
var _elm_lang$svg$Svg_Attributes$result = _elm_lang$virtual_dom$VirtualDom$attribute('result');
var _elm_lang$svg$Svg_Attributes$restart = _elm_lang$virtual_dom$VirtualDom$attribute('restart');
var _elm_lang$svg$Svg_Attributes$requiredFeatures = _elm_lang$virtual_dom$VirtualDom$attribute('requiredFeatures');
var _elm_lang$svg$Svg_Attributes$requiredExtensions = _elm_lang$virtual_dom$VirtualDom$attribute('requiredExtensions');
var _elm_lang$svg$Svg_Attributes$repeatDur = _elm_lang$virtual_dom$VirtualDom$attribute('repeatDur');
var _elm_lang$svg$Svg_Attributes$repeatCount = _elm_lang$virtual_dom$VirtualDom$attribute('repeatCount');
var _elm_lang$svg$Svg_Attributes$renderingIntent = _elm_lang$virtual_dom$VirtualDom$attribute('rendering-intent');
var _elm_lang$svg$Svg_Attributes$refY = _elm_lang$virtual_dom$VirtualDom$attribute('refY');
var _elm_lang$svg$Svg_Attributes$refX = _elm_lang$virtual_dom$VirtualDom$attribute('refX');
var _elm_lang$svg$Svg_Attributes$radius = _elm_lang$virtual_dom$VirtualDom$attribute('radius');
var _elm_lang$svg$Svg_Attributes$r = _elm_lang$virtual_dom$VirtualDom$attribute('r');
var _elm_lang$svg$Svg_Attributes$primitiveUnits = _elm_lang$virtual_dom$VirtualDom$attribute('primitiveUnits');
var _elm_lang$svg$Svg_Attributes$preserveAspectRatio = _elm_lang$virtual_dom$VirtualDom$attribute('preserveAspectRatio');
var _elm_lang$svg$Svg_Attributes$preserveAlpha = _elm_lang$virtual_dom$VirtualDom$attribute('preserveAlpha');
var _elm_lang$svg$Svg_Attributes$pointsAtZ = _elm_lang$virtual_dom$VirtualDom$attribute('pointsAtZ');
var _elm_lang$svg$Svg_Attributes$pointsAtY = _elm_lang$virtual_dom$VirtualDom$attribute('pointsAtY');
var _elm_lang$svg$Svg_Attributes$pointsAtX = _elm_lang$virtual_dom$VirtualDom$attribute('pointsAtX');
var _elm_lang$svg$Svg_Attributes$points = _elm_lang$virtual_dom$VirtualDom$attribute('points');
var _elm_lang$svg$Svg_Attributes$pointOrder = _elm_lang$virtual_dom$VirtualDom$attribute('point-order');
var _elm_lang$svg$Svg_Attributes$patternUnits = _elm_lang$virtual_dom$VirtualDom$attribute('patternUnits');
var _elm_lang$svg$Svg_Attributes$patternTransform = _elm_lang$virtual_dom$VirtualDom$attribute('patternTransform');
var _elm_lang$svg$Svg_Attributes$patternContentUnits = _elm_lang$virtual_dom$VirtualDom$attribute('patternContentUnits');
var _elm_lang$svg$Svg_Attributes$pathLength = _elm_lang$virtual_dom$VirtualDom$attribute('pathLength');
var _elm_lang$svg$Svg_Attributes$path = _elm_lang$virtual_dom$VirtualDom$attribute('path');
var _elm_lang$svg$Svg_Attributes$panose1 = _elm_lang$virtual_dom$VirtualDom$attribute('panose-1');
var _elm_lang$svg$Svg_Attributes$overlineThickness = _elm_lang$virtual_dom$VirtualDom$attribute('overline-thickness');
var _elm_lang$svg$Svg_Attributes$overlinePosition = _elm_lang$virtual_dom$VirtualDom$attribute('overline-position');
var _elm_lang$svg$Svg_Attributes$origin = _elm_lang$virtual_dom$VirtualDom$attribute('origin');
var _elm_lang$svg$Svg_Attributes$orientation = _elm_lang$virtual_dom$VirtualDom$attribute('orientation');
var _elm_lang$svg$Svg_Attributes$orient = _elm_lang$virtual_dom$VirtualDom$attribute('orient');
var _elm_lang$svg$Svg_Attributes$order = _elm_lang$virtual_dom$VirtualDom$attribute('order');
var _elm_lang$svg$Svg_Attributes$operator = _elm_lang$virtual_dom$VirtualDom$attribute('operator');
var _elm_lang$svg$Svg_Attributes$offset = _elm_lang$virtual_dom$VirtualDom$attribute('offset');
var _elm_lang$svg$Svg_Attributes$numOctaves = _elm_lang$virtual_dom$VirtualDom$attribute('numOctaves');
var _elm_lang$svg$Svg_Attributes$name = _elm_lang$virtual_dom$VirtualDom$attribute('name');
var _elm_lang$svg$Svg_Attributes$mode = _elm_lang$virtual_dom$VirtualDom$attribute('mode');
var _elm_lang$svg$Svg_Attributes$min = _elm_lang$virtual_dom$VirtualDom$attribute('min');
var _elm_lang$svg$Svg_Attributes$method = _elm_lang$virtual_dom$VirtualDom$attribute('method');
var _elm_lang$svg$Svg_Attributes$media = _elm_lang$virtual_dom$VirtualDom$attribute('media');
var _elm_lang$svg$Svg_Attributes$max = _elm_lang$virtual_dom$VirtualDom$attribute('max');
var _elm_lang$svg$Svg_Attributes$mathematical = _elm_lang$virtual_dom$VirtualDom$attribute('mathematical');
var _elm_lang$svg$Svg_Attributes$maskUnits = _elm_lang$virtual_dom$VirtualDom$attribute('maskUnits');
var _elm_lang$svg$Svg_Attributes$maskContentUnits = _elm_lang$virtual_dom$VirtualDom$attribute('maskContentUnits');
var _elm_lang$svg$Svg_Attributes$markerWidth = _elm_lang$virtual_dom$VirtualDom$attribute('markerWidth');
var _elm_lang$svg$Svg_Attributes$markerUnits = _elm_lang$virtual_dom$VirtualDom$attribute('markerUnits');
var _elm_lang$svg$Svg_Attributes$markerHeight = _elm_lang$virtual_dom$VirtualDom$attribute('markerHeight');
var _elm_lang$svg$Svg_Attributes$local = _elm_lang$virtual_dom$VirtualDom$attribute('local');
var _elm_lang$svg$Svg_Attributes$limitingConeAngle = _elm_lang$virtual_dom$VirtualDom$attribute('limitingConeAngle');
var _elm_lang$svg$Svg_Attributes$lengthAdjust = _elm_lang$virtual_dom$VirtualDom$attribute('lengthAdjust');
var _elm_lang$svg$Svg_Attributes$lang = _elm_lang$virtual_dom$VirtualDom$attribute('lang');
var _elm_lang$svg$Svg_Attributes$keyTimes = _elm_lang$virtual_dom$VirtualDom$attribute('keyTimes');
var _elm_lang$svg$Svg_Attributes$keySplines = _elm_lang$virtual_dom$VirtualDom$attribute('keySplines');
var _elm_lang$svg$Svg_Attributes$keyPoints = _elm_lang$virtual_dom$VirtualDom$attribute('keyPoints');
var _elm_lang$svg$Svg_Attributes$kernelUnitLength = _elm_lang$virtual_dom$VirtualDom$attribute('kernelUnitLength');
var _elm_lang$svg$Svg_Attributes$kernelMatrix = _elm_lang$virtual_dom$VirtualDom$attribute('kernelMatrix');
var _elm_lang$svg$Svg_Attributes$k4 = _elm_lang$virtual_dom$VirtualDom$attribute('k4');
var _elm_lang$svg$Svg_Attributes$k3 = _elm_lang$virtual_dom$VirtualDom$attribute('k3');
var _elm_lang$svg$Svg_Attributes$k2 = _elm_lang$virtual_dom$VirtualDom$attribute('k2');
var _elm_lang$svg$Svg_Attributes$k1 = _elm_lang$virtual_dom$VirtualDom$attribute('k1');
var _elm_lang$svg$Svg_Attributes$k = _elm_lang$virtual_dom$VirtualDom$attribute('k');
var _elm_lang$svg$Svg_Attributes$intercept = _elm_lang$virtual_dom$VirtualDom$attribute('intercept');
var _elm_lang$svg$Svg_Attributes$in2 = _elm_lang$virtual_dom$VirtualDom$attribute('in2');
var _elm_lang$svg$Svg_Attributes$in_ = _elm_lang$virtual_dom$VirtualDom$attribute('in');
var _elm_lang$svg$Svg_Attributes$ideographic = _elm_lang$virtual_dom$VirtualDom$attribute('ideographic');
var _elm_lang$svg$Svg_Attributes$id = _elm_lang$virtual_dom$VirtualDom$attribute('id');
var _elm_lang$svg$Svg_Attributes$horizOriginY = _elm_lang$virtual_dom$VirtualDom$attribute('horiz-origin-y');
var _elm_lang$svg$Svg_Attributes$horizOriginX = _elm_lang$virtual_dom$VirtualDom$attribute('horiz-origin-x');
var _elm_lang$svg$Svg_Attributes$horizAdvX = _elm_lang$virtual_dom$VirtualDom$attribute('horiz-adv-x');
var _elm_lang$svg$Svg_Attributes$height = _elm_lang$virtual_dom$VirtualDom$attribute('height');
var _elm_lang$svg$Svg_Attributes$hanging = _elm_lang$virtual_dom$VirtualDom$attribute('hanging');
var _elm_lang$svg$Svg_Attributes$gradientUnits = _elm_lang$virtual_dom$VirtualDom$attribute('gradientUnits');
var _elm_lang$svg$Svg_Attributes$gradientTransform = _elm_lang$virtual_dom$VirtualDom$attribute('gradientTransform');
var _elm_lang$svg$Svg_Attributes$glyphRef = _elm_lang$virtual_dom$VirtualDom$attribute('glyphRef');
var _elm_lang$svg$Svg_Attributes$glyphName = _elm_lang$virtual_dom$VirtualDom$attribute('glyph-name');
var _elm_lang$svg$Svg_Attributes$g2 = _elm_lang$virtual_dom$VirtualDom$attribute('g2');
var _elm_lang$svg$Svg_Attributes$g1 = _elm_lang$virtual_dom$VirtualDom$attribute('g1');
var _elm_lang$svg$Svg_Attributes$fy = _elm_lang$virtual_dom$VirtualDom$attribute('fy');
var _elm_lang$svg$Svg_Attributes$fx = _elm_lang$virtual_dom$VirtualDom$attribute('fx');
var _elm_lang$svg$Svg_Attributes$from = _elm_lang$virtual_dom$VirtualDom$attribute('from');
var _elm_lang$svg$Svg_Attributes$format = _elm_lang$virtual_dom$VirtualDom$attribute('format');
var _elm_lang$svg$Svg_Attributes$filterUnits = _elm_lang$virtual_dom$VirtualDom$attribute('filterUnits');
var _elm_lang$svg$Svg_Attributes$filterRes = _elm_lang$virtual_dom$VirtualDom$attribute('filterRes');
var _elm_lang$svg$Svg_Attributes$externalResourcesRequired = _elm_lang$virtual_dom$VirtualDom$attribute('externalResourcesRequired');
var _elm_lang$svg$Svg_Attributes$exponent = _elm_lang$virtual_dom$VirtualDom$attribute('exponent');
var _elm_lang$svg$Svg_Attributes$end = _elm_lang$virtual_dom$VirtualDom$attribute('end');
var _elm_lang$svg$Svg_Attributes$elevation = _elm_lang$virtual_dom$VirtualDom$attribute('elevation');
var _elm_lang$svg$Svg_Attributes$edgeMode = _elm_lang$virtual_dom$VirtualDom$attribute('edgeMode');
var _elm_lang$svg$Svg_Attributes$dy = _elm_lang$virtual_dom$VirtualDom$attribute('dy');
var _elm_lang$svg$Svg_Attributes$dx = _elm_lang$virtual_dom$VirtualDom$attribute('dx');
var _elm_lang$svg$Svg_Attributes$dur = _elm_lang$virtual_dom$VirtualDom$attribute('dur');
var _elm_lang$svg$Svg_Attributes$divisor = _elm_lang$virtual_dom$VirtualDom$attribute('divisor');
var _elm_lang$svg$Svg_Attributes$diffuseConstant = _elm_lang$virtual_dom$VirtualDom$attribute('diffuseConstant');
var _elm_lang$svg$Svg_Attributes$descent = _elm_lang$virtual_dom$VirtualDom$attribute('descent');
var _elm_lang$svg$Svg_Attributes$decelerate = _elm_lang$virtual_dom$VirtualDom$attribute('decelerate');
var _elm_lang$svg$Svg_Attributes$d = _elm_lang$virtual_dom$VirtualDom$attribute('d');
var _elm_lang$svg$Svg_Attributes$cy = _elm_lang$virtual_dom$VirtualDom$attribute('cy');
var _elm_lang$svg$Svg_Attributes$cx = _elm_lang$virtual_dom$VirtualDom$attribute('cx');
var _elm_lang$svg$Svg_Attributes$contentStyleType = _elm_lang$virtual_dom$VirtualDom$attribute('contentStyleType');
var _elm_lang$svg$Svg_Attributes$contentScriptType = _elm_lang$virtual_dom$VirtualDom$attribute('contentScriptType');
var _elm_lang$svg$Svg_Attributes$clipPathUnits = _elm_lang$virtual_dom$VirtualDom$attribute('clipPathUnits');
var _elm_lang$svg$Svg_Attributes$class = _elm_lang$virtual_dom$VirtualDom$attribute('class');
var _elm_lang$svg$Svg_Attributes$capHeight = _elm_lang$virtual_dom$VirtualDom$attribute('cap-height');
var _elm_lang$svg$Svg_Attributes$calcMode = _elm_lang$virtual_dom$VirtualDom$attribute('calcMode');
var _elm_lang$svg$Svg_Attributes$by = _elm_lang$virtual_dom$VirtualDom$attribute('by');
var _elm_lang$svg$Svg_Attributes$bias = _elm_lang$virtual_dom$VirtualDom$attribute('bias');
var _elm_lang$svg$Svg_Attributes$begin = _elm_lang$virtual_dom$VirtualDom$attribute('begin');
var _elm_lang$svg$Svg_Attributes$bbox = _elm_lang$virtual_dom$VirtualDom$attribute('bbox');
var _elm_lang$svg$Svg_Attributes$baseProfile = _elm_lang$virtual_dom$VirtualDom$attribute('baseProfile');
var _elm_lang$svg$Svg_Attributes$baseFrequency = _elm_lang$virtual_dom$VirtualDom$attribute('baseFrequency');
var _elm_lang$svg$Svg_Attributes$azimuth = _elm_lang$virtual_dom$VirtualDom$attribute('azimuth');
var _elm_lang$svg$Svg_Attributes$autoReverse = _elm_lang$virtual_dom$VirtualDom$attribute('autoReverse');
var _elm_lang$svg$Svg_Attributes$attributeType = _elm_lang$virtual_dom$VirtualDom$attribute('attributeType');
var _elm_lang$svg$Svg_Attributes$attributeName = _elm_lang$virtual_dom$VirtualDom$attribute('attributeName');
var _elm_lang$svg$Svg_Attributes$ascent = _elm_lang$virtual_dom$VirtualDom$attribute('ascent');
var _elm_lang$svg$Svg_Attributes$arabicForm = _elm_lang$virtual_dom$VirtualDom$attribute('arabic-form');
var _elm_lang$svg$Svg_Attributes$amplitude = _elm_lang$virtual_dom$VirtualDom$attribute('amplitude');
var _elm_lang$svg$Svg_Attributes$allowReorder = _elm_lang$virtual_dom$VirtualDom$attribute('allowReorder');
var _elm_lang$svg$Svg_Attributes$alphabetic = _elm_lang$virtual_dom$VirtualDom$attribute('alphabetic');
var _elm_lang$svg$Svg_Attributes$additive = _elm_lang$virtual_dom$VirtualDom$attribute('additive');
var _elm_lang$svg$Svg_Attributes$accumulate = _elm_lang$virtual_dom$VirtualDom$attribute('accumulate');
var _elm_lang$svg$Svg_Attributes$accelerate = _elm_lang$virtual_dom$VirtualDom$attribute('accelerate');
var _elm_lang$svg$Svg_Attributes$accentHeight = _elm_lang$virtual_dom$VirtualDom$attribute('accent-height');

var _myrho$elm_round$Round$funNum = F3(
	function (fun, s, fl) {
		return A2(
			_elm_lang$core$Maybe$withDefault,
			1 / 0,
			_elm_lang$core$Result$toMaybe(
				_elm_lang$core$String$toFloat(
					A2(fun, s, fl))));
	});
var _myrho$elm_round$Round$splitComma = function (str) {
	var _p0 = A2(_elm_lang$core$String$split, '.', str);
	if (_p0.ctor === '::') {
		if (_p0._1.ctor === '::') {
			return {ctor: '_Tuple2', _0: _p0._0, _1: _p0._1._0};
		} else {
			return {ctor: '_Tuple2', _0: _p0._0, _1: '0'};
		}
	} else {
		return {ctor: '_Tuple2', _0: '0', _1: '0'};
	}
};
var _myrho$elm_round$Round$toDecimal = function (fl) {
	var _p1 = A2(
		_elm_lang$core$String$split,
		'e',
		_elm_lang$core$Basics$toString(fl));
	if (_p1.ctor === '::') {
		if (_p1._1.ctor === '::') {
			var _p4 = _p1._1._0;
			var _p2 = function () {
				var hasSign = _elm_lang$core$Native_Utils.cmp(fl, 0) < 0;
				var _p3 = _myrho$elm_round$Round$splitComma(_p1._0);
				var b = _p3._0;
				var a = _p3._1;
				return {
					ctor: '_Tuple3',
					_0: hasSign ? '-' : '',
					_1: hasSign ? A2(_elm_lang$core$String$dropLeft, 1, b) : b,
					_2: a
				};
			}();
			var sign = _p2._0;
			var before = _p2._1;
			var after = _p2._2;
			var e = A2(
				_elm_lang$core$Maybe$withDefault,
				0,
				_elm_lang$core$Result$toMaybe(
					_elm_lang$core$String$toInt(
						A2(_elm_lang$core$String$startsWith, '+', _p4) ? A2(_elm_lang$core$String$dropLeft, 1, _p4) : _p4)));
			var newBefore = (_elm_lang$core$Native_Utils.cmp(e, 0) > -1) ? before : ((_elm_lang$core$Native_Utils.cmp(
				_elm_lang$core$Basics$abs(e),
				_elm_lang$core$String$length(before)) < 0) ? A2(
				_elm_lang$core$Basics_ops['++'],
				A2(
					_elm_lang$core$String$left,
					_elm_lang$core$String$length(before) - _elm_lang$core$Basics$abs(e),
					before),
				A2(
					_elm_lang$core$Basics_ops['++'],
					'.',
					A2(
						_elm_lang$core$String$right,
						_elm_lang$core$Basics$abs(e),
						before))) : A2(
				_elm_lang$core$Basics_ops['++'],
				'0.',
				A2(
					_elm_lang$core$Basics_ops['++'],
					A2(
						_elm_lang$core$String$repeat,
						_elm_lang$core$Basics$abs(e) - _elm_lang$core$String$length(before),
						'0'),
					before)));
			var newAfter = (_elm_lang$core$Native_Utils.cmp(e, 0) < 1) ? after : ((_elm_lang$core$Native_Utils.cmp(
				e,
				_elm_lang$core$String$length(after)) < 0) ? A2(
				_elm_lang$core$Basics_ops['++'],
				A2(_elm_lang$core$String$left, e, after),
				A2(
					_elm_lang$core$Basics_ops['++'],
					'.',
					A2(
						_elm_lang$core$String$right,
						_elm_lang$core$String$length(after) - e,
						after))) : A2(
				_elm_lang$core$Basics_ops['++'],
				after,
				A2(
					_elm_lang$core$String$repeat,
					e - _elm_lang$core$String$length(after),
					'0')));
			return A2(
				_elm_lang$core$Basics_ops['++'],
				sign,
				A2(_elm_lang$core$Basics_ops['++'], newBefore, newAfter));
		} else {
			return _p1._0;
		}
	} else {
		return '';
	}
};
var _myrho$elm_round$Round$truncate = function (n) {
	return (_elm_lang$core$Native_Utils.cmp(n, 0) < 0) ? _elm_lang$core$Basics$ceiling(n) : _elm_lang$core$Basics$floor(n);
};
var _myrho$elm_round$Round$roundFun = F3(
	function (functor, s, fl) {
		if (_elm_lang$core$Native_Utils.eq(s, 0)) {
			return _elm_lang$core$Basics$toString(
				functor(fl));
		} else {
			if (_elm_lang$core$Native_Utils.cmp(s, 0) < 0) {
				return function (r) {
					return (!_elm_lang$core$Native_Utils.eq(r, '0')) ? A2(
						_elm_lang$core$Basics_ops['++'],
						r,
						A2(
							_elm_lang$core$String$repeat,
							_elm_lang$core$Basics$abs(s),
							'0')) : r;
				}(
					A3(
						_myrho$elm_round$Round$roundFun,
						functor,
						0,
						A2(
							F2(
								function (x, y) {
									return x / y;
								}),
							fl,
							A2(
								F2(
									function (x, y) {
										return Math.pow(x, y);
									}),
								10,
								_elm_lang$core$Basics$abs(
									_elm_lang$core$Basics$toFloat(s))))));
			} else {
				var dd = (_elm_lang$core$Native_Utils.cmp(fl, 0) < 0) ? 2 : 1;
				var n = (_elm_lang$core$Native_Utils.cmp(fl, 0) < 0) ? -1 : 1;
				var e = Math.pow(10, s);
				var _p5 = _myrho$elm_round$Round$splitComma(
					_myrho$elm_round$Round$toDecimal(fl));
				var before = _p5._0;
				var after = _p5._1;
				var a = A3(
					_elm_lang$core$String$padRight,
					s + 1,
					_elm_lang$core$Native_Utils.chr('0'),
					after);
				var b = A2(_elm_lang$core$String$left, s, a);
				var c = A2(_elm_lang$core$String$dropLeft, s, a);
				var f = functor(
					A2(
						_elm_lang$core$Maybe$withDefault,
						_elm_lang$core$Basics$toFloat(e),
						_elm_lang$core$Result$toMaybe(
							_elm_lang$core$String$toFloat(
								A2(
									_elm_lang$core$Basics_ops['++'],
									(_elm_lang$core$Native_Utils.cmp(fl, 0) < 0) ? '-' : '',
									A2(
										_elm_lang$core$Basics_ops['++'],
										'1',
										A2(
											_elm_lang$core$Basics_ops['++'],
											b,
											A2(_elm_lang$core$Basics_ops['++'], '.', c))))))));
				var g = A2(
					_elm_lang$core$String$dropLeft,
					dd,
					_elm_lang$core$Basics$toString(f));
				var h = _myrho$elm_round$Round$truncate(fl) + (_elm_lang$core$Native_Utils.eq(f - (e * n), e * n) ? ((_elm_lang$core$Native_Utils.cmp(fl, 0) < 0) ? -1 : 1) : 0);
				var j = _elm_lang$core$Basics$toString(h);
				var i = (_elm_lang$core$Native_Utils.eq(j, '0') && ((!_elm_lang$core$Native_Utils.eq(f - (e * n), 0)) && ((_elm_lang$core$Native_Utils.cmp(fl, 0) < 0) && (_elm_lang$core$Native_Utils.cmp(fl, -1) > 0)))) ? A2(_elm_lang$core$Basics_ops['++'], '-', j) : j;
				return A2(
					_elm_lang$core$Basics_ops['++'],
					i,
					A2(_elm_lang$core$Basics_ops['++'], '.', g));
			}
		}
	});
var _myrho$elm_round$Round$round = _myrho$elm_round$Round$roundFun(_elm_lang$core$Basics$round);
var _myrho$elm_round$Round$roundNum = _myrho$elm_round$Round$funNum(_myrho$elm_round$Round$round);
var _myrho$elm_round$Round$ceiling = _myrho$elm_round$Round$roundFun(_elm_lang$core$Basics$ceiling);
var _myrho$elm_round$Round$ceilingNum = _myrho$elm_round$Round$funNum(_myrho$elm_round$Round$ceiling);
var _myrho$elm_round$Round$floor = _myrho$elm_round$Round$roundFun(_elm_lang$core$Basics$floor);
var _myrho$elm_round$Round$floorCom = F2(
	function (s, fl) {
		return (_elm_lang$core$Native_Utils.cmp(fl, 0) < 0) ? A2(_myrho$elm_round$Round$ceiling, s, fl) : A2(_myrho$elm_round$Round$floor, s, fl);
	});
var _myrho$elm_round$Round$floorNumCom = _myrho$elm_round$Round$funNum(_myrho$elm_round$Round$floorCom);
var _myrho$elm_round$Round$ceilingCom = F2(
	function (s, fl) {
		return (_elm_lang$core$Native_Utils.cmp(fl, 0) < 0) ? A2(_myrho$elm_round$Round$floor, s, fl) : A2(_myrho$elm_round$Round$ceiling, s, fl);
	});
var _myrho$elm_round$Round$ceilingNumCom = _myrho$elm_round$Round$funNum(_myrho$elm_round$Round$ceilingCom);
var _myrho$elm_round$Round$floorNum = _myrho$elm_round$Round$funNum(_myrho$elm_round$Round$floor);
var _myrho$elm_round$Round$roundCom = _myrho$elm_round$Round$roundFun(
	function (fl) {
		var dec = fl - _elm_lang$core$Basics$toFloat(
			_myrho$elm_round$Round$truncate(fl));
		return (_elm_lang$core$Native_Utils.cmp(dec, 0.5) > -1) ? _elm_lang$core$Basics$ceiling(fl) : ((_elm_lang$core$Native_Utils.cmp(dec, -0.5) < 1) ? _elm_lang$core$Basics$floor(fl) : _elm_lang$core$Basics$round(fl));
	});
var _myrho$elm_round$Round$roundNumCom = _myrho$elm_round$Round$funNum(_myrho$elm_round$Round$roundCom);

var _terezka$elm_plot$Internal_Colors$darkGrey = '#a3a3a3';
var _terezka$elm_plot$Internal_Colors$grey = '#e3e3e3';
var _terezka$elm_plot$Internal_Colors$transparent = 'transparent';
var _terezka$elm_plot$Internal_Colors$blueStroke = '#cfd8ea';
var _terezka$elm_plot$Internal_Colors$blueFill = '#e4eeff';
var _terezka$elm_plot$Internal_Colors$pinkStroke = '#ff9edf';
var _terezka$elm_plot$Internal_Colors$pinkFill = 'rgba(253, 185, 231, 0.5)';

var _terezka$elm_plot$Internal_Draw$sign = function (x) {
	return (_elm_lang$core$Native_Utils.cmp(x, 0) < 0) ? -1 : 1;
};
var _terezka$elm_plot$Internal_Draw$slope2 = F3(
	function (point0, point1, t) {
		var h = point1.x - point0.x;
		return (!_elm_lang$core$Native_Utils.eq(h, 0)) ? ((((3 * (point1.y - point0.y)) / h) - t) / 2) : t;
	});
var _terezka$elm_plot$Internal_Draw$toH = F2(
	function (h0, h1) {
		return _elm_lang$core$Native_Utils.eq(h0, 0) ? ((_elm_lang$core$Native_Utils.cmp(h1, 0) < 0) ? (0 * -1) : h1) : h0;
	});
var _terezka$elm_plot$Internal_Draw$slope3 = F3(
	function (point0, point1, point2) {
		var h1 = point2.x - point1.x;
		var h0 = point1.x - point0.x;
		var s0h = A2(_terezka$elm_plot$Internal_Draw$toH, h0, h1);
		var s0 = (point1.y - point0.y) / s0h;
		var s1h = A2(_terezka$elm_plot$Internal_Draw$toH, h1, h0);
		var s1 = (point2.y - point1.y) / s1h;
		var p = ((s0 * h1) + (s1 * h0)) / (h0 + h1);
		var slope = (_terezka$elm_plot$Internal_Draw$sign(s0) + _terezka$elm_plot$Internal_Draw$sign(s1)) * A2(
			_elm_lang$core$Basics$min,
			A2(
				_elm_lang$core$Basics$min,
				_elm_lang$core$Basics$abs(s0),
				_elm_lang$core$Basics$abs(s1)),
			0.5 * _elm_lang$core$Basics$abs(p));
		return _elm_lang$core$Basics$isNaN(slope) ? 0 : slope;
	});
var _terezka$elm_plot$Internal_Draw$boolToString = function (bool) {
	return bool ? '0' : '1';
};
var _terezka$elm_plot$Internal_Draw$pointToString = function (_p0) {
	var _p1 = _p0;
	return A2(
		_elm_lang$core$Basics_ops['++'],
		_elm_lang$core$Basics$toString(_p1.x),
		A2(
			_elm_lang$core$Basics_ops['++'],
			' ',
			_elm_lang$core$Basics$toString(_p1.y)));
};
var _terezka$elm_plot$Internal_Draw$pointsToString = function (points) {
	return A2(
		_elm_lang$core$String$join,
		',',
		A2(_elm_lang$core$List$map, _terezka$elm_plot$Internal_Draw$pointToString, points));
};
var _terezka$elm_plot$Internal_Draw$joinCommands = function (commands) {
	return A2(_elm_lang$core$String$join, ' ', commands);
};
var _terezka$elm_plot$Internal_Draw$yClosestToZero = function (_p2) {
	var _p3 = _p2;
	var _p4 = _p3.y;
	return A3(_elm_lang$core$Basics$clamp, _p4.min, _p4.max, 0);
};
var _terezka$elm_plot$Internal_Draw$length = function (axis) {
	return (axis.length - axis.marginLower) - axis.marginUpper;
};
var _terezka$elm_plot$Internal_Draw$range = function (axis) {
	return (!_elm_lang$core$Native_Utils.eq(axis.max - axis.min, 0)) ? (axis.max - axis.min) : 1;
};
var _terezka$elm_plot$Internal_Draw$scaleValue = F2(
	function (axis, value) {
		return (value * _terezka$elm_plot$Internal_Draw$length(axis)) / _terezka$elm_plot$Internal_Draw$range(axis);
	});
var _terezka$elm_plot$Internal_Draw$toSVGX = F2(
	function (_p5, value) {
		var _p6 = _p5;
		var _p7 = _p6.x;
		return A2(_terezka$elm_plot$Internal_Draw$scaleValue, _p7, value - _p7.min) + _p7.marginLower;
	});
var _terezka$elm_plot$Internal_Draw$toSVGY = F2(
	function (_p8, value) {
		var _p9 = _p8;
		var _p10 = _p9.y;
		return A2(_terezka$elm_plot$Internal_Draw$scaleValue, _p10, _p10.max - value) + _p10.marginLower;
	});
var _terezka$elm_plot$Internal_Draw$place = F4(
	function (plot, _p11, offsetX, offsetY) {
		var _p12 = _p11;
		return _elm_lang$svg$Svg_Attributes$transform(
			A2(
				_elm_lang$core$Basics_ops['++'],
				'translate(',
				A2(
					_elm_lang$core$Basics_ops['++'],
					_elm_lang$core$Basics$toString(
						A2(_terezka$elm_plot$Internal_Draw$toSVGX, plot, _p12.x) + offsetX),
					A2(
						_elm_lang$core$Basics_ops['++'],
						',',
						A2(
							_elm_lang$core$Basics_ops['++'],
							_elm_lang$core$Basics$toString(
								A2(_terezka$elm_plot$Internal_Draw$toSVGY, plot, _p12.y) + offsetY),
							')')))));
	});
var _terezka$elm_plot$Internal_Draw$unScaleValue = F2(
	function (axis, v) {
		return (v * _terezka$elm_plot$Internal_Draw$range(axis)) / _terezka$elm_plot$Internal_Draw$length(axis);
	});
var _terezka$elm_plot$Internal_Draw$toUnSVGX = F2(
	function (_p13, value) {
		var _p14 = _p13;
		var _p15 = _p14.x;
		return A2(_terezka$elm_plot$Internal_Draw$unScaleValue, _p15, value - _p15.marginLower) + _p15.min;
	});
var _terezka$elm_plot$Internal_Draw$toUnSVGY = F2(
	function (_p16, value) {
		var _p17 = _p16;
		var _p18 = _p17.y;
		return (_terezka$elm_plot$Internal_Draw$range(_p18) - A2(_terezka$elm_plot$Internal_Draw$unScaleValue, _p18, value - _p18.marginLower)) + _p18.min;
	});
var _terezka$elm_plot$Internal_Draw$Point = F2(
	function (a, b) {
		return {x: a, y: b};
	});
var _terezka$elm_plot$Internal_Draw$stringifyCommand = function (command) {
	var _p19 = command;
	switch (_p19.ctor) {
		case 'Move':
			return A2(
				_elm_lang$core$Basics_ops['++'],
				'M',
				_terezka$elm_plot$Internal_Draw$pointToString(
					A2(_terezka$elm_plot$Internal_Draw$Point, _p19._0, _p19._1)));
		case 'Line':
			return A2(
				_elm_lang$core$Basics_ops['++'],
				'L',
				_terezka$elm_plot$Internal_Draw$pointToString(
					A2(_terezka$elm_plot$Internal_Draw$Point, _p19._0, _p19._1)));
		case 'HorizontalLine':
			return A2(
				_elm_lang$core$Basics_ops['++'],
				'H',
				_elm_lang$core$Basics$toString(_p19._0));
		case 'VerticalLine':
			return A2(
				_elm_lang$core$Basics_ops['++'],
				'V',
				_elm_lang$core$Basics$toString(_p19._0));
		case 'CubicBeziers':
			return A2(
				_elm_lang$core$Basics_ops['++'],
				'C',
				_terezka$elm_plot$Internal_Draw$pointsToString(
					{
						ctor: '::',
						_0: A2(_terezka$elm_plot$Internal_Draw$Point, _p19._0, _p19._1),
						_1: {
							ctor: '::',
							_0: A2(_terezka$elm_plot$Internal_Draw$Point, _p19._2, _p19._3),
							_1: {
								ctor: '::',
								_0: A2(_terezka$elm_plot$Internal_Draw$Point, _p19._4, _p19._5),
								_1: {ctor: '[]'}
							}
						}
					}));
		case 'CubicBeziersShort':
			return A2(
				_elm_lang$core$Basics_ops['++'],
				'Q',
				_terezka$elm_plot$Internal_Draw$pointsToString(
					{
						ctor: '::',
						_0: A2(_terezka$elm_plot$Internal_Draw$Point, _p19._0, _p19._1),
						_1: {
							ctor: '::',
							_0: A2(_terezka$elm_plot$Internal_Draw$Point, _p19._2, _p19._3),
							_1: {ctor: '[]'}
						}
					}));
		case 'QuadraticBeziers':
			return A2(
				_elm_lang$core$Basics_ops['++'],
				'Q',
				_terezka$elm_plot$Internal_Draw$pointsToString(
					{
						ctor: '::',
						_0: A2(_terezka$elm_plot$Internal_Draw$Point, _p19._0, _p19._1),
						_1: {
							ctor: '::',
							_0: A2(_terezka$elm_plot$Internal_Draw$Point, _p19._2, _p19._3),
							_1: {ctor: '[]'}
						}
					}));
		case 'QuadraticBeziersShort':
			return A2(
				_elm_lang$core$Basics_ops['++'],
				'T',
				_terezka$elm_plot$Internal_Draw$pointToString(
					A2(_terezka$elm_plot$Internal_Draw$Point, _p19._0, _p19._1)));
		case 'Arc':
			return A2(
				_elm_lang$core$Basics_ops['++'],
				'A',
				_terezka$elm_plot$Internal_Draw$joinCommands(
					{
						ctor: '::',
						_0: _terezka$elm_plot$Internal_Draw$pointToString(
							A2(_terezka$elm_plot$Internal_Draw$Point, _p19._0, _p19._1)),
						_1: {
							ctor: '::',
							_0: _elm_lang$core$Basics$toString(_p19._2),
							_1: {
								ctor: '::',
								_0: _terezka$elm_plot$Internal_Draw$boolToString(_p19._3),
								_1: {
									ctor: '::',
									_0: _terezka$elm_plot$Internal_Draw$boolToString(_p19._4),
									_1: {
										ctor: '::',
										_0: _terezka$elm_plot$Internal_Draw$pointToString(
											A2(_terezka$elm_plot$Internal_Draw$Point, _p19._5, _p19._6)),
										_1: {ctor: '[]'}
									}
								}
							}
						}
					}));
		default:
			return 'Z';
	}
};
var _terezka$elm_plot$Internal_Draw$draw = F2(
	function (attributes, commands) {
		return A2(
			_elm_lang$svg$Svg$path,
			{
				ctor: '::',
				_0: _elm_lang$svg$Svg_Attributes$d(
					_terezka$elm_plot$Internal_Draw$joinCommands(
						A2(_elm_lang$core$List$map, _terezka$elm_plot$Internal_Draw$stringifyCommand, commands))),
				_1: attributes
			},
			{ctor: '[]'});
	});
var _terezka$elm_plot$Internal_Draw$AxisSummary = F8(
	function (a, b, c, d, e, f, g, h) {
		return {min: a, max: b, dataMin: c, dataMax: d, marginLower: e, marginUpper: f, length: g, all: h};
	});
var _terezka$elm_plot$Internal_Draw$PlotSummary = F2(
	function (a, b) {
		return {x: a, y: b};
	});
var _terezka$elm_plot$Internal_Draw$Close = {ctor: 'Close'};
var _terezka$elm_plot$Internal_Draw$Arc = F7(
	function (a, b, c, d, e, f, g) {
		return {ctor: 'Arc', _0: a, _1: b, _2: c, _3: d, _4: e, _5: f, _6: g};
	});
var _terezka$elm_plot$Internal_Draw$QuadraticBeziersShort = F2(
	function (a, b) {
		return {ctor: 'QuadraticBeziersShort', _0: a, _1: b};
	});
var _terezka$elm_plot$Internal_Draw$QuadraticBeziers = F4(
	function (a, b, c, d) {
		return {ctor: 'QuadraticBeziers', _0: a, _1: b, _2: c, _3: d};
	});
var _terezka$elm_plot$Internal_Draw$CubicBeziersShort = F4(
	function (a, b, c, d) {
		return {ctor: 'CubicBeziersShort', _0: a, _1: b, _2: c, _3: d};
	});
var _terezka$elm_plot$Internal_Draw$CubicBeziers = F6(
	function (a, b, c, d, e, f) {
		return {ctor: 'CubicBeziers', _0: a, _1: b, _2: c, _3: d, _4: e, _5: f};
	});
var _terezka$elm_plot$Internal_Draw$monotoneXCurve = F4(
	function (point0, point1, tangent0, tangent1) {
		var dx = (point1.x - point0.x) / 3;
		return {
			ctor: '::',
			_0: A6(_terezka$elm_plot$Internal_Draw$CubicBeziers, point0.x + dx, point0.y + (dx * tangent0), point1.x - dx, point1.y - (dx * tangent1), point1.x, point1.y),
			_1: {ctor: '[]'}
		};
	});
var _terezka$elm_plot$Internal_Draw$monotoneXNext = F3(
	function (points, tangent0, commands) {
		monotoneXNext:
		while (true) {
			var _p20 = points;
			if ((_p20.ctor === '::') && (_p20._1.ctor === '::')) {
				if (_p20._1._1.ctor === '::') {
					var _p23 = _p20._1._1._0;
					var _p22 = _p20._1._0;
					var _p21 = _p20._0;
					var tangent1 = A3(_terezka$elm_plot$Internal_Draw$slope3, _p21, _p22, _p23);
					var nextCommands = A2(
						_elm_lang$core$Basics_ops['++'],
						commands,
						A4(_terezka$elm_plot$Internal_Draw$monotoneXCurve, _p21, _p22, tangent0, tangent1));
					var _v9 = {
						ctor: '::',
						_0: _p22,
						_1: {ctor: '::', _0: _p23, _1: _p20._1._1._1}
					},
						_v10 = tangent1,
						_v11 = nextCommands;
					points = _v9;
					tangent0 = _v10;
					commands = _v11;
					continue monotoneXNext;
				} else {
					var _p25 = _p20._1._0;
					var _p24 = _p20._0;
					var tangent1 = A3(_terezka$elm_plot$Internal_Draw$slope3, _p24, _p25, _p25);
					return A2(
						_elm_lang$core$Basics_ops['++'],
						commands,
						A4(_terezka$elm_plot$Internal_Draw$monotoneXCurve, _p24, _p25, tangent0, tangent1));
				}
			} else {
				return commands;
			}
		}
	});
var _terezka$elm_plot$Internal_Draw$monotoneXBegin = function (points) {
	var _p26 = points;
	if (((_p26.ctor === '::') && (_p26._1.ctor === '::')) && (_p26._1._1.ctor === '::')) {
		var _p29 = _p26._1._1._0;
		var _p28 = _p26._1._0;
		var _p27 = _p26._0;
		var tangent1 = A3(_terezka$elm_plot$Internal_Draw$slope3, _p27, _p28, _p29);
		var tangent0 = A3(_terezka$elm_plot$Internal_Draw$slope2, _p27, _p28, tangent1);
		return A2(
			_elm_lang$core$Basics_ops['++'],
			A4(_terezka$elm_plot$Internal_Draw$monotoneXCurve, _p27, _p28, tangent0, tangent1),
			A3(
				_terezka$elm_plot$Internal_Draw$monotoneXNext,
				{
					ctor: '::',
					_0: _p28,
					_1: {ctor: '::', _0: _p29, _1: _p26._1._1._1}
				},
				tangent1,
				{ctor: '[]'}));
	} else {
		return {ctor: '[]'};
	}
};
var _terezka$elm_plot$Internal_Draw$VerticalLine = function (a) {
	return {ctor: 'VerticalLine', _0: a};
};
var _terezka$elm_plot$Internal_Draw$HorizontalLine = function (a) {
	return {ctor: 'HorizontalLine', _0: a};
};
var _terezka$elm_plot$Internal_Draw$Line = F2(
	function (a, b) {
		return {ctor: 'Line', _0: a, _1: b};
	});
var _terezka$elm_plot$Internal_Draw$lineCommand = function (_p30) {
	var _p31 = _p30;
	return A2(_terezka$elm_plot$Internal_Draw$Line, _p31.x, _p31.y);
};
var _terezka$elm_plot$Internal_Draw$areaEnd = F2(
	function (plot, points) {
		var _p32 = _elm_lang$core$List$head(
			_elm_lang$core$List$reverse(points));
		if (_p32.ctor === 'Just') {
			return {
				ctor: '::',
				_0: A2(
					_terezka$elm_plot$Internal_Draw$Line,
					_p32._0.x,
					_terezka$elm_plot$Internal_Draw$yClosestToZero(plot)),
				_1: {ctor: '[]'}
			};
		} else {
			return {ctor: '[]'};
		}
	});
var _terezka$elm_plot$Internal_Draw$Move = F2(
	function (a, b) {
		return {ctor: 'Move', _0: a, _1: b};
	});
var _terezka$elm_plot$Internal_Draw$lineBegin = F2(
	function (plot, points) {
		var _p33 = points;
		if (_p33.ctor === '::') {
			return {
				ctor: '::',
				_0: A2(_terezka$elm_plot$Internal_Draw$Move, _p33._0.x, _p33._0.y),
				_1: {ctor: '[]'}
			};
		} else {
			return {ctor: '[]'};
		}
	});
var _terezka$elm_plot$Internal_Draw$areaBegin = F2(
	function (plot, points) {
		var _p34 = points;
		if (_p34.ctor === '::') {
			var _p35 = _p34._0.x;
			return {
				ctor: '::',
				_0: A2(
					_terezka$elm_plot$Internal_Draw$Move,
					_p35,
					_terezka$elm_plot$Internal_Draw$yClosestToZero(plot)),
				_1: {
					ctor: '::',
					_0: A2(_terezka$elm_plot$Internal_Draw$Line, _p35, _p34._0.y),
					_1: {ctor: '[]'}
				}
			};
		} else {
			return {ctor: '[]'};
		}
	});
var _terezka$elm_plot$Internal_Draw$translateCommand = F2(
	function (plot, command) {
		var _p36 = command;
		switch (_p36.ctor) {
			case 'Move':
				return A2(
					_terezka$elm_plot$Internal_Draw$Move,
					A2(_terezka$elm_plot$Internal_Draw$toSVGX, plot, _p36._0),
					A2(_terezka$elm_plot$Internal_Draw$toSVGY, plot, _p36._1));
			case 'Line':
				return A2(
					_terezka$elm_plot$Internal_Draw$Line,
					A2(_terezka$elm_plot$Internal_Draw$toSVGX, plot, _p36._0),
					A2(_terezka$elm_plot$Internal_Draw$toSVGY, plot, _p36._1));
			case 'HorizontalLine':
				return _terezka$elm_plot$Internal_Draw$HorizontalLine(
					A2(_terezka$elm_plot$Internal_Draw$toSVGX, plot, _p36._0));
			case 'VerticalLine':
				return _terezka$elm_plot$Internal_Draw$VerticalLine(
					A2(_terezka$elm_plot$Internal_Draw$toSVGY, plot, _p36._0));
			case 'CubicBeziers':
				return A6(
					_terezka$elm_plot$Internal_Draw$CubicBeziers,
					A2(_terezka$elm_plot$Internal_Draw$toSVGX, plot, _p36._0),
					A2(_terezka$elm_plot$Internal_Draw$toSVGY, plot, _p36._1),
					A2(_terezka$elm_plot$Internal_Draw$toSVGX, plot, _p36._2),
					A2(_terezka$elm_plot$Internal_Draw$toSVGY, plot, _p36._3),
					A2(_terezka$elm_plot$Internal_Draw$toSVGX, plot, _p36._4),
					A2(_terezka$elm_plot$Internal_Draw$toSVGY, plot, _p36._5));
			case 'CubicBeziersShort':
				return A4(
					_terezka$elm_plot$Internal_Draw$CubicBeziersShort,
					A2(_terezka$elm_plot$Internal_Draw$toSVGX, plot, _p36._0),
					A2(_terezka$elm_plot$Internal_Draw$toSVGY, plot, _p36._1),
					A2(_terezka$elm_plot$Internal_Draw$toSVGX, plot, _p36._2),
					A2(_terezka$elm_plot$Internal_Draw$toSVGY, plot, _p36._3));
			case 'QuadraticBeziers':
				return A4(
					_terezka$elm_plot$Internal_Draw$QuadraticBeziers,
					A2(_terezka$elm_plot$Internal_Draw$toSVGX, plot, _p36._0),
					A2(_terezka$elm_plot$Internal_Draw$toSVGY, plot, _p36._1),
					A2(_terezka$elm_plot$Internal_Draw$toSVGX, plot, _p36._2),
					A2(_terezka$elm_plot$Internal_Draw$toSVGY, plot, _p36._3));
			case 'QuadraticBeziersShort':
				return A2(
					_terezka$elm_plot$Internal_Draw$QuadraticBeziersShort,
					A2(_terezka$elm_plot$Internal_Draw$toSVGX, plot, _p36._0),
					A2(_terezka$elm_plot$Internal_Draw$toSVGY, plot, _p36._1));
			case 'Arc':
				return A7(
					_terezka$elm_plot$Internal_Draw$Arc,
					A2(_terezka$elm_plot$Internal_Draw$toSVGX, plot, _p36._0),
					A2(_terezka$elm_plot$Internal_Draw$toSVGY, plot, _p36._1),
					_p36._2,
					_p36._3,
					_p36._4,
					A2(_terezka$elm_plot$Internal_Draw$toSVGX, plot, _p36._5),
					A2(_terezka$elm_plot$Internal_Draw$toSVGY, plot, _p36._6));
			default:
				return _terezka$elm_plot$Internal_Draw$Close;
		}
	});
var _terezka$elm_plot$Internal_Draw$linear = F2(
	function (plot, points) {
		return A2(
			_elm_lang$core$List$map,
			_terezka$elm_plot$Internal_Draw$translateCommand(plot),
			A2(
				_elm_lang$core$Basics_ops['++'],
				A2(_terezka$elm_plot$Internal_Draw$lineBegin, plot, points),
				A2(_elm_lang$core$List$map, _terezka$elm_plot$Internal_Draw$lineCommand, points)));
	});
var _terezka$elm_plot$Internal_Draw$linearArea = F2(
	function (plot, points) {
		return A2(
			_elm_lang$core$List$map,
			_terezka$elm_plot$Internal_Draw$translateCommand(plot),
			A2(
				_elm_lang$core$Basics_ops['++'],
				A2(_terezka$elm_plot$Internal_Draw$areaBegin, plot, points),
				A2(
					_elm_lang$core$Basics_ops['++'],
					A2(_elm_lang$core$List$map, _terezka$elm_plot$Internal_Draw$lineCommand, points),
					A2(_terezka$elm_plot$Internal_Draw$areaEnd, plot, points))));
	});
var _terezka$elm_plot$Internal_Draw$monotoneX = F2(
	function (plot, points) {
		return A2(
			_elm_lang$core$List$map,
			_terezka$elm_plot$Internal_Draw$translateCommand(plot),
			A2(
				_elm_lang$core$Basics_ops['++'],
				A2(_terezka$elm_plot$Internal_Draw$lineBegin, plot, points),
				_terezka$elm_plot$Internal_Draw$monotoneXBegin(points)));
	});
var _terezka$elm_plot$Internal_Draw$monotoneXArea = F2(
	function (plot, points) {
		return A2(
			_elm_lang$core$List$map,
			_terezka$elm_plot$Internal_Draw$translateCommand(plot),
			A2(
				_elm_lang$core$Basics_ops['++'],
				A2(_terezka$elm_plot$Internal_Draw$areaBegin, plot, points),
				A2(
					_elm_lang$core$Basics_ops['++'],
					_terezka$elm_plot$Internal_Draw$monotoneXBegin(points),
					A2(_terezka$elm_plot$Internal_Draw$areaEnd, plot, points))));
	});

var _terezka$elm_plot$Plot$point = function (_p0) {
	var _p1 = _p0;
	return A2(_terezka$elm_plot$Internal_Draw$Point, _p1.x, _p1.y);
};
var _terezka$elm_plot$Plot$points = _elm_lang$core$List$map(_terezka$elm_plot$Plot$point);
var _terezka$elm_plot$Plot$niceInterval = F3(
	function (min, max, total) {
		var range = _elm_lang$core$Basics$abs(max - min);
		var delta0 = range / _elm_lang$core$Basics$toFloat(total);
		var mag = _elm_lang$core$Basics$floor(
			A2(_elm_lang$core$Basics$logBase, 10, delta0));
		var magPow = _elm_lang$core$Basics$toFloat(
			Math.pow(10, mag));
		var magMsd = _elm_lang$core$Basics$round(delta0 / magPow);
		var magMsdFinal = (_elm_lang$core$Native_Utils.cmp(magMsd, 5) > 0) ? 10 : ((_elm_lang$core$Native_Utils.cmp(magMsd, 2) > 0) ? 5 : ((_elm_lang$core$Native_Utils.cmp(magMsd, 1) > 0) ? 1 : magMsd));
		return _elm_lang$core$Basics$toFloat(magMsdFinal) * magPow;
	});
var _terezka$elm_plot$Plot$count = F4(
	function (delta, lowest, range, firstValue) {
		return _elm_lang$core$Basics$floor(
			(range - (_elm_lang$core$Basics$abs(lowest) - _elm_lang$core$Basics$abs(firstValue))) / delta);
	});
var _terezka$elm_plot$Plot$ceilToNearest = F2(
	function (precision, value) {
		return _elm_lang$core$Basics$toFloat(
			_elm_lang$core$Basics$ceiling(value / precision)) * precision;
	});
var _terezka$elm_plot$Plot$firstValue = F2(
	function (delta, lowest) {
		return A2(_terezka$elm_plot$Plot$ceilToNearest, delta, lowest);
	});
var _terezka$elm_plot$Plot$deltaPrecision = function (delta) {
	return _elm_lang$core$Basics$abs(
		A2(
			_elm_lang$core$Basics$min,
			0,
			A2(
				F2(
					function (x, y) {
						return x - y;
					}),
				1,
				_elm_lang$core$String$length(
					A2(
						_elm_lang$core$Maybe$withDefault,
						'',
						_elm_lang$core$List$head(
							A2(
								_elm_lang$core$List$map,
								function (_) {
									return _.match;
								},
								A3(
									_elm_lang$core$Regex$find,
									_elm_lang$core$Regex$AtMost(1),
									_elm_lang$core$Regex$regex('\\.[0-9]*'),
									_elm_lang$core$Basics$toString(delta)))))))));
};
var _terezka$elm_plot$Plot$tickPosition = F3(
	function (delta, firstValue, index) {
		return A2(
			_elm_lang$core$Result$withDefault,
			0,
			_elm_lang$core$String$toFloat(
				A2(
					_myrho$elm_round$Round$round,
					_terezka$elm_plot$Plot$deltaPrecision(delta),
					firstValue + (_elm_lang$core$Basics$toFloat(index) * delta))));
	});
var _terezka$elm_plot$Plot$remove = F2(
	function (banned, values) {
		return A2(
			_elm_lang$core$List$filter,
			function (v) {
				return !_elm_lang$core$Native_Utils.eq(v, banned);
			},
			values);
	});
var _terezka$elm_plot$Plot$interval = F3(
	function (offset, delta, _p2) {
		var _p3 = _p2;
		var _p4 = _p3.min;
		var value = A2(_terezka$elm_plot$Plot$firstValue, delta, _p4) + offset;
		var range = _elm_lang$core$Basics$abs(_p4 - _p3.max);
		var indexes = A2(
			_elm_lang$core$List$range,
			0,
			A4(_terezka$elm_plot$Plot$count, delta, _p4, range, value));
		return A2(
			_elm_lang$core$List$map,
			A2(_terezka$elm_plot$Plot$tickPosition, delta, value),
			indexes);
	});
var _terezka$elm_plot$Plot$decentPositions = function (summary) {
	return (_elm_lang$core$Native_Utils.cmp(summary.length, 600) > 0) ? A3(
		_terezka$elm_plot$Plot$interval,
		0,
		A3(_terezka$elm_plot$Plot$niceInterval, summary.min, summary.max, 10),
		summary) : A3(
		_terezka$elm_plot$Plot$interval,
		0,
		A3(_terezka$elm_plot$Plot$niceInterval, summary.min, summary.max, 5),
		summary);
};
var _terezka$elm_plot$Plot$viewLabel = F2(
	function (attributes, string) {
		return A2(
			_elm_lang$svg$Svg$text_,
			attributes,
			{
				ctor: '::',
				_0: A2(
					_elm_lang$svg$Svg$tspan,
					{ctor: '[]'},
					{
						ctor: '::',
						_0: _elm_lang$svg$Svg$text(string),
						_1: {ctor: '[]'}
					}),
				_1: {ctor: '[]'}
			});
	});
var _terezka$elm_plot$Plot$viewTickInner = F3(
	function (attributes, width, height) {
		return A2(
			_elm_lang$svg$Svg$line,
			{
				ctor: '::',
				_0: _elm_lang$svg$Svg_Attributes$x2(
					_elm_lang$core$Basics$toString(width)),
				_1: {
					ctor: '::',
					_0: _elm_lang$svg$Svg_Attributes$y2(
						_elm_lang$core$Basics$toString(height)),
					_1: attributes
				}
			},
			{ctor: '[]'});
	});
var _terezka$elm_plot$Plot$viewAxisLine = F3(
	function (summary, at, axisLine) {
		var _p5 = axisLine;
		if (_p5.ctor === 'Just') {
			return A2(
				_terezka$elm_plot$Internal_Draw$draw,
				_p5._0.attributes,
				A2(
					_terezka$elm_plot$Internal_Draw$linear,
					summary,
					{
						ctor: '::',
						_0: at(_p5._0.start),
						_1: {
							ctor: '::',
							_0: at(_p5._0.end),
							_1: {ctor: '[]'}
						}
					}));
		} else {
			return _elm_lang$svg$Svg$text('');
		}
	});
var _terezka$elm_plot$Plot$viewGlitterLines = F2(
	function (summary, _p6) {
		var _p7 = _p6;
		return {
			ctor: '::',
			_0: A3(
				_terezka$elm_plot$Plot$viewAxisLine,
				summary,
				function (y) {
					return {x: _p7.x, y: y};
				},
				A2(
					_elm_lang$core$Maybe$map,
					function (toLine) {
						return toLine(summary.y);
					},
					_p7.xLine)),
			_1: {
				ctor: '::',
				_0: A3(
					_terezka$elm_plot$Plot$viewAxisLine,
					summary,
					function (x) {
						return {x: x, y: _p7.y};
					},
					A2(
						_elm_lang$core$Maybe$map,
						function (toLine) {
							return toLine(summary.x);
						},
						_p7.yLine)),
				_1: {ctor: '[]'}
			}
		};
	});
var _terezka$elm_plot$Plot$viewActualVerticalAxis = F3(
	function (summary, _p8, glitterTicks) {
		var _p9 = _p8;
		var _p14 = _p9.flipAnchor;
		var anchorOfLabel = _p14 ? 'text-anchor: start;' : 'text-anchor: end;';
		var positionOfLabel = _p14 ? 10 : -10;
		var lengthOfTick = function (length) {
			return _p14 ? length : (0 - length);
		};
		var at = function (y) {
			return {
				x: A2(_p9.position, summary.x.min, summary.x.max),
				y: y
			};
		};
		var viewTickLine = function (_p10) {
			var _p11 = _p10;
			return A2(
				_elm_lang$svg$Svg$g,
				{
					ctor: '::',
					_0: A4(
						_terezka$elm_plot$Internal_Draw$place,
						summary,
						at(_p11.position),
						0,
						0),
					_1: {ctor: '[]'}
				},
				{
					ctor: '::',
					_0: A3(
						_terezka$elm_plot$Plot$viewTickInner,
						_p11.attributes,
						lengthOfTick(_p11.length),
						0),
					_1: {ctor: '[]'}
				});
		};
		var viewLabel = function (_p12) {
			var _p13 = _p12;
			return A2(
				_elm_lang$svg$Svg$g,
				{
					ctor: '::',
					_0: A4(
						_terezka$elm_plot$Internal_Draw$place,
						summary,
						at(_p13.position),
						positionOfLabel,
						5),
					_1: {
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$style(anchorOfLabel),
						_1: {ctor: '[]'}
					}
				},
				{
					ctor: '::',
					_0: _p13.view,
					_1: {ctor: '[]'}
				});
		};
		return A2(
			_elm_lang$svg$Svg$g,
			{
				ctor: '::',
				_0: _elm_lang$svg$Svg_Attributes$class('elm-plot__vertical-axis'),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: A3(_terezka$elm_plot$Plot$viewAxisLine, summary, at, _p9.axisLine),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$svg$Svg$g,
						{
							ctor: '::',
							_0: _elm_lang$svg$Svg_Attributes$class('elm-plot__ticks'),
							_1: {ctor: '[]'}
						},
						A2(
							_elm_lang$core$List$map,
							viewTickLine,
							A2(_elm_lang$core$Basics_ops['++'], _p9.ticks, glitterTicks))),
					_1: {
						ctor: '::',
						_0: A2(
							_elm_lang$svg$Svg$g,
							{
								ctor: '::',
								_0: _elm_lang$svg$Svg_Attributes$class('elm-plot__labels'),
								_1: {ctor: '[]'}
							},
							A2(_elm_lang$core$List$map, viewLabel, _p9.labels)),
						_1: {ctor: '[]'}
					}
				}
			});
	});
var _terezka$elm_plot$Plot$viewVerticalAxis = F3(
	function (summary, axis, moreTicks) {
		var _p15 = axis;
		if (_p15.ctor === 'Axis') {
			return _elm_lang$core$Maybe$Just(
				A2(
					_elm_lang$svg$Svg$map,
					_elm_lang$core$Basics$never,
					A3(
						_terezka$elm_plot$Plot$viewActualVerticalAxis,
						summary,
						_p15._0(summary.y),
						moreTicks)));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _terezka$elm_plot$Plot$viewActualHorizontalAxis = F4(
	function (summary, _p16, glitterLabels, glitterTicks) {
		var _p17 = _p16;
		var _p22 = _p17.flipAnchor;
		var positionOfLabel = _p22 ? -10 : 20;
		var lengthOfTick = function (length) {
			return _p22 ? (0 - length) : length;
		};
		var at = function (x) {
			return {
				x: x,
				y: A2(_p17.position, summary.y.min, summary.y.max)
			};
		};
		var viewTickLine = function (_p18) {
			var _p19 = _p18;
			return A2(
				_elm_lang$svg$Svg$g,
				{
					ctor: '::',
					_0: A4(
						_terezka$elm_plot$Internal_Draw$place,
						summary,
						at(_p19.position),
						0,
						0),
					_1: {ctor: '[]'}
				},
				{
					ctor: '::',
					_0: A3(
						_terezka$elm_plot$Plot$viewTickInner,
						_p19.attributes,
						0,
						lengthOfTick(_p19.length)),
					_1: {ctor: '[]'}
				});
		};
		var viewLabel = function (_p20) {
			var _p21 = _p20;
			return A2(
				_elm_lang$svg$Svg$g,
				{
					ctor: '::',
					_0: A4(
						_terezka$elm_plot$Internal_Draw$place,
						summary,
						at(_p21.position),
						0,
						positionOfLabel),
					_1: {
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$style('text-anchor: middle;'),
						_1: {ctor: '[]'}
					}
				},
				{
					ctor: '::',
					_0: _p21.view,
					_1: {ctor: '[]'}
				});
		};
		return A2(
			_elm_lang$svg$Svg$g,
			{
				ctor: '::',
				_0: _elm_lang$svg$Svg_Attributes$class('elm-plot__horizontal-axis'),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: A3(_terezka$elm_plot$Plot$viewAxisLine, summary, at, _p17.axisLine),
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$svg$Svg$g,
						{
							ctor: '::',
							_0: _elm_lang$svg$Svg_Attributes$class('elm-plot__ticks'),
							_1: {ctor: '[]'}
						},
						A2(
							_elm_lang$core$List$map,
							viewTickLine,
							A2(_elm_lang$core$Basics_ops['++'], _p17.ticks, glitterTicks))),
					_1: {
						ctor: '::',
						_0: A2(
							_elm_lang$svg$Svg$g,
							{
								ctor: '::',
								_0: _elm_lang$svg$Svg_Attributes$class('elm-plot__labels'),
								_1: {ctor: '[]'}
							},
							A2(
								_elm_lang$core$List$map,
								viewLabel,
								A2(_elm_lang$core$Basics_ops['++'], _p17.labels, glitterLabels))),
						_1: {ctor: '[]'}
					}
				}
			});
	});
var _terezka$elm_plot$Plot$viewHorizontalAxis = F4(
	function (summary, axis, moreLabels, moreTicks) {
		var _p23 = axis;
		if (_p23.ctor === 'Axis') {
			return _elm_lang$core$Maybe$Just(
				A2(
					_elm_lang$svg$Svg$map,
					_elm_lang$core$Basics$never,
					A4(
						_terezka$elm_plot$Plot$viewActualHorizontalAxis,
						summary,
						_p23._0(summary.x),
						moreLabels,
						moreTicks)));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _terezka$elm_plot$Plot$viewTriangle = function (color) {
	return A2(
		_elm_lang$svg$Svg$polygon,
		{
			ctor: '::',
			_0: _elm_lang$svg$Svg_Attributes$points('0,-5 5,5 -5,5'),
			_1: {
				ctor: '::',
				_0: _elm_lang$svg$Svg_Attributes$transform('translate(0, -2.5)'),
				_1: {
					ctor: '::',
					_0: _elm_lang$svg$Svg_Attributes$fill(color),
					_1: {ctor: '[]'}
				}
			}
		},
		{ctor: '[]'});
};
var _terezka$elm_plot$Plot$viewDiamond = F3(
	function (width, height, color) {
		return A2(
			_elm_lang$svg$Svg$rect,
			{
				ctor: '::',
				_0: _elm_lang$svg$Svg_Attributes$width(
					_elm_lang$core$Basics$toString(width)),
				_1: {
					ctor: '::',
					_0: _elm_lang$svg$Svg_Attributes$height(
						_elm_lang$core$Basics$toString(height)),
					_1: {
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$transform('rotate(45)'),
						_1: {
							ctor: '::',
							_0: _elm_lang$svg$Svg_Attributes$x(
								_elm_lang$core$Basics$toString((0 - width) / 2)),
							_1: {
								ctor: '::',
								_0: _elm_lang$svg$Svg_Attributes$y(
									_elm_lang$core$Basics$toString((0 - height) / 2)),
								_1: {
									ctor: '::',
									_0: _elm_lang$svg$Svg_Attributes$stroke('transparent'),
									_1: {
										ctor: '::',
										_0: _elm_lang$svg$Svg_Attributes$fill(color),
										_1: {ctor: '[]'}
									}
								}
							}
						}
					}
				}
			},
			{ctor: '[]'});
	});
var _terezka$elm_plot$Plot$viewSquare = F2(
	function (width, color) {
		return A2(
			_elm_lang$svg$Svg$rect,
			{
				ctor: '::',
				_0: _elm_lang$svg$Svg_Attributes$width(
					_elm_lang$core$Basics$toString(width)),
				_1: {
					ctor: '::',
					_0: _elm_lang$svg$Svg_Attributes$height(
						_elm_lang$core$Basics$toString(width)),
					_1: {
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$x(
							_elm_lang$core$Basics$toString((0 - width) / 2)),
						_1: {
							ctor: '::',
							_0: _elm_lang$svg$Svg_Attributes$y(
								_elm_lang$core$Basics$toString((0 - width) / 2)),
							_1: {
								ctor: '::',
								_0: _elm_lang$svg$Svg_Attributes$stroke('transparent'),
								_1: {
									ctor: '::',
									_0: _elm_lang$svg$Svg_Attributes$fill(color),
									_1: {ctor: '[]'}
								}
							}
						}
					}
				}
			},
			{ctor: '[]'});
	});
var _terezka$elm_plot$Plot$viewCircle = F2(
	function (radius, color) {
		return A2(
			_elm_lang$svg$Svg$circle,
			{
				ctor: '::',
				_0: _elm_lang$svg$Svg_Attributes$r(
					_elm_lang$core$Basics$toString(radius)),
				_1: {
					ctor: '::',
					_0: _elm_lang$svg$Svg_Attributes$stroke('transparent'),
					_1: {
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$fill(color),
						_1: {ctor: '[]'}
					}
				}
			},
			{ctor: '[]'});
	});
var _terezka$elm_plot$Plot$viewDataPoint = F2(
	function (plotSummary, _p24) {
		var _p25 = _p24;
		var _p26 = _p25.view;
		if (_p26.ctor === 'Nothing') {
			return _elm_lang$core$Maybe$Nothing;
		} else {
			return _elm_lang$core$Maybe$Just(
				A2(
					_elm_lang$svg$Svg$g,
					{
						ctor: '::',
						_0: A4(
							_terezka$elm_plot$Internal_Draw$place,
							plotSummary,
							{x: _p25.x, y: _p25.y},
							0,
							0),
						_1: {ctor: '[]'}
					},
					{
						ctor: '::',
						_0: _p26._0,
						_1: {ctor: '[]'}
					}));
		}
	});
var _terezka$elm_plot$Plot$viewDataPoints = F2(
	function (plotSummary, dataPoints) {
		return A2(
			_elm_lang$svg$Svg$g,
			{
				ctor: '::',
				_0: _elm_lang$svg$Svg_Attributes$class('elm-plot__series__points'),
				_1: {ctor: '[]'}
			},
			A2(
				_elm_lang$core$List$filterMap,
				_elm_lang$core$Basics$identity,
				A2(
					_elm_lang$core$List$map,
					_terezka$elm_plot$Plot$viewDataPoint(plotSummary),
					dataPoints)));
	});
var _terezka$elm_plot$Plot$viewActualHorizontalGrid = F2(
	function (summary, gridLines) {
		var viewGridLine = function (_p27) {
			var _p28 = _p27;
			var _p29 = _p28.position;
			return A2(
				_terezka$elm_plot$Internal_Draw$draw,
				_p28.attributes,
				A2(
					_terezka$elm_plot$Internal_Draw$linear,
					summary,
					{
						ctor: '::',
						_0: {x: summary.x.min, y: _p29},
						_1: {
							ctor: '::',
							_0: {x: summary.x.max, y: _p29},
							_1: {ctor: '[]'}
						}
					}));
		};
		return A2(
			_elm_lang$svg$Svg$g,
			{
				ctor: '::',
				_0: _elm_lang$svg$Svg_Attributes$class('elm-plot__vertical-grid'),
				_1: {ctor: '[]'}
			},
			A2(_elm_lang$core$List$map, viewGridLine, gridLines));
	});
var _terezka$elm_plot$Plot$viewHorizontalGrid = F2(
	function (summary, grid) {
		var _p30 = grid;
		if (_p30.ctor === 'Grid') {
			return _elm_lang$core$Maybe$Just(
				A2(
					_elm_lang$svg$Svg$map,
					_elm_lang$core$Basics$never,
					A2(
						_terezka$elm_plot$Plot$viewActualHorizontalGrid,
						summary,
						_p30._0(summary.y))));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _terezka$elm_plot$Plot$viewActualVerticalGrid = F2(
	function (summary, gridLines) {
		var viewGridLine = function (_p31) {
			var _p32 = _p31;
			var _p33 = _p32.position;
			return A2(
				_terezka$elm_plot$Internal_Draw$draw,
				_p32.attributes,
				A2(
					_terezka$elm_plot$Internal_Draw$linear,
					summary,
					{
						ctor: '::',
						_0: {x: _p33, y: summary.y.min},
						_1: {
							ctor: '::',
							_0: {x: _p33, y: summary.y.max},
							_1: {ctor: '[]'}
						}
					}));
		};
		return A2(
			_elm_lang$svg$Svg$g,
			{
				ctor: '::',
				_0: _elm_lang$svg$Svg_Attributes$class('elm-plot__horizontal-grid'),
				_1: {ctor: '[]'}
			},
			A2(_elm_lang$core$List$map, viewGridLine, gridLines));
	});
var _terezka$elm_plot$Plot$viewVerticalGrid = F2(
	function (summary, grid) {
		var _p34 = grid;
		if (_p34.ctor === 'Grid') {
			return _elm_lang$core$Maybe$Just(
				A2(
					_elm_lang$svg$Svg$map,
					_elm_lang$core$Basics$never,
					A2(
						_terezka$elm_plot$Plot$viewActualVerticalGrid,
						summary,
						_p34._0(summary.x))));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _terezka$elm_plot$Plot$defaultPlotSummary = {
	x: {
		min: 0.0,
		max: 1.0,
		all: {ctor: '[]'}
	},
	y: {
		min: 0.0,
		max: 1.0,
		all: {ctor: '[]'}
	}
};
var _terezka$elm_plot$Plot$toPlotSummary = F3(
	function (customizations, toNiceReach, points) {
		var foldAxis = F2(
			function (summary, v) {
				return {
					min: A2(_elm_lang$core$Basics$min, summary.min, v),
					max: A2(_elm_lang$core$Basics$max, summary.max, v),
					all: {ctor: '::', _0: v, _1: summary.all}
				};
			});
		var foldPlot = F2(
			function (_p35, result) {
				var _p36 = _p35;
				var _p40 = _p36.y;
				var _p39 = _p36.x;
				var _p37 = result;
				if (_p37.ctor === 'Nothing') {
					return _elm_lang$core$Maybe$Just(
						{
							x: {
								min: _p39,
								max: _p39,
								all: {
									ctor: '::',
									_0: _p39,
									_1: {ctor: '[]'}
								}
							},
							y: {
								min: _p40,
								max: _p40,
								all: {
									ctor: '::',
									_0: _p40,
									_1: {ctor: '[]'}
								}
							}
						});
				} else {
					var _p38 = _p37._0;
					return _elm_lang$core$Maybe$Just(
						{
							x: A2(foldAxis, _p38.x, _p39),
							y: A2(foldAxis, _p38.y, _p40)
						});
				}
			});
		var plotSummary = toNiceReach(
			A2(
				_elm_lang$core$Maybe$withDefault,
				_terezka$elm_plot$Plot$defaultPlotSummary,
				A3(_elm_lang$core$List$foldl, foldPlot, _elm_lang$core$Maybe$Nothing, points)));
		return {
			x: {
				min: customizations.toRangeLowest(plotSummary.x.min),
				max: customizations.toRangeHighest(plotSummary.x.max),
				dataMin: plotSummary.x.min,
				dataMax: plotSummary.x.max,
				length: _elm_lang$core$Basics$toFloat(customizations.width),
				marginLower: _elm_lang$core$Basics$toFloat(customizations.margin.left),
				marginUpper: _elm_lang$core$Basics$toFloat(customizations.margin.right),
				all: _elm_lang$core$List$sort(plotSummary.x.all)
			},
			y: {
				min: customizations.toDomainLowest(plotSummary.y.min),
				max: customizations.toDomainHighest(plotSummary.y.max),
				dataMin: plotSummary.y.min,
				dataMax: plotSummary.y.max,
				length: _elm_lang$core$Basics$toFloat(customizations.height),
				marginLower: _elm_lang$core$Basics$toFloat(customizations.margin.top),
				marginUpper: _elm_lang$core$Basics$toFloat(customizations.margin.bottom),
				all: plotSummary.y.all
			}
		};
	});
var _terezka$elm_plot$Plot$diff = F2(
	function (a, b) {
		return _elm_lang$core$Basics$abs(a - b);
	});
var _terezka$elm_plot$Plot$toNearestX = F2(
	function (summary, exactX) {
		var updateIfCloser = F2(
			function (closest, x) {
				return (_elm_lang$core$Native_Utils.cmp(
					A2(_terezka$elm_plot$Plot$diff, x, exactX),
					A2(_terezka$elm_plot$Plot$diff, closest, exactX)) > 0) ? closest : x;
			});
		var $default = A2(
			_elm_lang$core$Maybe$withDefault,
			0,
			_elm_lang$core$List$head(summary.x.all));
		return A3(_elm_lang$core$List$foldl, updateIfCloser, $default, summary.x.all);
	});
var _terezka$elm_plot$Plot$unScalePoint = F4(
	function (summary, mouseX, mouseY, _p41) {
		var _p42 = _p41;
		return _elm_lang$core$Maybe$Just(
			{
				x: A2(
					_terezka$elm_plot$Plot$toNearestX,
					summary,
					A2(_terezka$elm_plot$Internal_Draw$toUnSVGX, summary, (summary.x.length * (mouseX - _p42.left)) / _p42.width)),
				y: A3(
					_elm_lang$core$Basics$clamp,
					summary.y.min,
					summary.y.max,
					A2(_terezka$elm_plot$Internal_Draw$toUnSVGY, summary, (summary.y.length * (mouseY - _p42.top)) / _p42.height))
			});
	});
var _terezka$elm_plot$Plot$plotPosition = _elm_lang$core$Json_Decode$oneOf(
	{
		ctor: '::',
		_0: _debois$elm_dom$DOM$boundingClientRect,
		_1: {
			ctor: '::',
			_0: _elm_lang$core$Json_Decode$lazy(
				function (_p43) {
					return _debois$elm_dom$DOM$parentElement(_terezka$elm_plot$Plot$plotPosition);
				}),
			_1: {ctor: '[]'}
		}
	});
var _terezka$elm_plot$Plot$handleHint = F2(
	function (summary, toMsg) {
		return A4(
			_elm_lang$core$Json_Decode$map3,
			F3(
				function (x, y, r) {
					return toMsg(
						A4(_terezka$elm_plot$Plot$unScalePoint, summary, x, y, r));
				}),
			A2(_elm_lang$core$Json_Decode$field, 'clientX', _elm_lang$core$Json_Decode$float),
			A2(_elm_lang$core$Json_Decode$field, 'clientY', _elm_lang$core$Json_Decode$float),
			_debois$elm_dom$DOM$target(_terezka$elm_plot$Plot$plotPosition));
	});
var _terezka$elm_plot$Plot$viewActualJunk = F2(
	function (summary, _p44) {
		var _p45 = _p44;
		return A2(
			_elm_lang$svg$Svg$g,
			{
				ctor: '::',
				_0: A4(
					_terezka$elm_plot$Internal_Draw$place,
					summary,
					{x: _p45.x, y: _p45.y},
					0,
					0),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: _p45.view,
				_1: {ctor: '[]'}
			});
	});
var _terezka$elm_plot$Plot$innerAttributes = function (customizations) {
	return A2(
		_elm_lang$core$Basics_ops['++'],
		customizations.attributes,
		{
			ctor: '::',
			_0: _elm_lang$svg$Svg_Attributes$viewBox(
				A2(
					_elm_lang$core$Basics_ops['++'],
					'0 0 ',
					A2(
						_elm_lang$core$Basics_ops['++'],
						_elm_lang$core$Basics$toString(customizations.width),
						A2(
							_elm_lang$core$Basics_ops['++'],
							' ',
							_elm_lang$core$Basics$toString(customizations.height))))),
			_1: {ctor: '[]'}
		});
};
var _terezka$elm_plot$Plot$containerAttributes = F2(
	function (customizations, summary) {
		var _p46 = customizations.onHover;
		if (_p46.ctor === 'Just') {
			var _p47 = _p46._0;
			return {
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html_Events$on,
					'mousemove',
					A2(_terezka$elm_plot$Plot$handleHint, summary, _p47)),
				_1: {
					ctor: '::',
					_0: _elm_lang$html$Html_Events$onMouseLeave(
						_p47(_elm_lang$core$Maybe$Nothing)),
					_1: {
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$id(customizations.id),
						_1: {
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$style(
								{
									ctor: '::',
									_0: {ctor: '_Tuple2', _0: 'position', _1: 'relative'},
									_1: {
										ctor: '::',
										_0: {ctor: '_Tuple2', _0: 'margin', _1: '0 auto'},
										_1: {ctor: '[]'}
									}
								}),
							_1: {ctor: '[]'}
						}
					}
				}
			};
		} else {
			return {
				ctor: '::',
				_0: _elm_lang$svg$Svg_Attributes$id(customizations.id),
				_1: {
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$style(
						{
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'position', _1: 'relative'},
							_1: {
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: 'margin', _1: '0 auto'},
								_1: {ctor: '[]'}
							}
						}),
					_1: {ctor: '[]'}
				}
			};
		}
	});
var _terezka$elm_plot$Plot$toClipPathId = function (_p48) {
	var _p49 = _p48;
	return A2(_elm_lang$core$Basics_ops['++'], 'elm-plot__clip-path__', _p49.id);
};
var _terezka$elm_plot$Plot$viewInterpolation = F7(
	function (customizations, summary, toLine, toArea, area, attributes, dataPoints) {
		var _p50 = area;
		if (_p50.ctor === 'Nothing') {
			return A2(
				_terezka$elm_plot$Internal_Draw$draw,
				{
					ctor: '::',
					_0: _elm_lang$svg$Svg_Attributes$fill(_terezka$elm_plot$Internal_Colors$transparent),
					_1: {
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$stroke(_terezka$elm_plot$Internal_Colors$pinkStroke),
						_1: {
							ctor: '::',
							_0: _elm_lang$svg$Svg_Attributes$class('elm-plot__series__interpolation'),
							_1: {
								ctor: '::',
								_0: _elm_lang$svg$Svg_Attributes$clipPath(
									A2(
										_elm_lang$core$Basics_ops['++'],
										'url(#',
										A2(
											_elm_lang$core$Basics_ops['++'],
											_terezka$elm_plot$Plot$toClipPathId(customizations),
											')'))),
								_1: attributes
							}
						}
					}
				},
				A2(
					toLine,
					summary,
					_terezka$elm_plot$Plot$points(dataPoints)));
		} else {
			return A2(
				_terezka$elm_plot$Internal_Draw$draw,
				{
					ctor: '::',
					_0: _elm_lang$svg$Svg_Attributes$fill(_p50._0),
					_1: {
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$stroke(_terezka$elm_plot$Internal_Colors$pinkStroke),
						_1: {
							ctor: '::',
							_0: _elm_lang$svg$Svg_Attributes$class('elm-plot__series__interpolation'),
							_1: {
								ctor: '::',
								_0: _elm_lang$svg$Svg_Attributes$clipPath(
									A2(
										_elm_lang$core$Basics_ops['++'],
										'url(#',
										A2(
											_elm_lang$core$Basics_ops['++'],
											_terezka$elm_plot$Plot$toClipPathId(customizations),
											')'))),
								_1: attributes
							}
						}
					}
				},
				A2(
					toArea,
					summary,
					_terezka$elm_plot$Plot$points(dataPoints)));
		}
	});
var _terezka$elm_plot$Plot$viewPath = F4(
	function (customizations, plotSummary, interpolation, dataPoints) {
		var _p51 = interpolation;
		switch (_p51.ctor) {
			case 'None':
				return A2(
					_elm_lang$svg$Svg$path,
					{ctor: '[]'},
					{ctor: '[]'});
			case 'Linear':
				return A7(_terezka$elm_plot$Plot$viewInterpolation, customizations, plotSummary, _terezka$elm_plot$Internal_Draw$linear, _terezka$elm_plot$Internal_Draw$linearArea, _p51._0, _p51._1, dataPoints);
			default:
				return A7(_terezka$elm_plot$Plot$viewInterpolation, customizations, plotSummary, _terezka$elm_plot$Internal_Draw$monotoneX, _terezka$elm_plot$Internal_Draw$monotoneXArea, _p51._0, _p51._1, dataPoints);
		}
	});
var _terezka$elm_plot$Plot$viewASeries = F4(
	function (customizations, plotSummary, _p52, dataPoints) {
		var _p53 = _p52;
		return A2(
			_elm_lang$svg$Svg$g,
			{
				ctor: '::',
				_0: _elm_lang$svg$Svg_Attributes$class('elm-plot__series'),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$svg$Svg$map,
					_elm_lang$core$Basics$never,
					A4(_terezka$elm_plot$Plot$viewPath, customizations, plotSummary, _p53.interpolation, dataPoints)),
				_1: {
					ctor: '::',
					_0: A2(_terezka$elm_plot$Plot$viewDataPoints, plotSummary, dataPoints),
					_1: {ctor: '[]'}
				}
			});
	});
var _terezka$elm_plot$Plot$defineClipPath = F2(
	function (customizations, summary) {
		return A2(
			_elm_lang$svg$Svg$defs,
			{ctor: '[]'},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$svg$Svg$clipPath,
					{
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$id(
							_terezka$elm_plot$Plot$toClipPathId(customizations)),
						_1: {ctor: '[]'}
					},
					{
						ctor: '::',
						_0: A2(
							_elm_lang$svg$Svg$rect,
							{
								ctor: '::',
								_0: _elm_lang$svg$Svg_Attributes$x(
									_elm_lang$core$Basics$toString(summary.x.marginLower)),
								_1: {
									ctor: '::',
									_0: _elm_lang$svg$Svg_Attributes$y(
										_elm_lang$core$Basics$toString(summary.y.marginLower)),
									_1: {
										ctor: '::',
										_0: _elm_lang$svg$Svg_Attributes$width(
											_elm_lang$core$Basics$toString(
												_terezka$elm_plot$Internal_Draw$length(summary.x))),
										_1: {
											ctor: '::',
											_0: _elm_lang$svg$Svg_Attributes$height(
												_elm_lang$core$Basics$toString(
													_terezka$elm_plot$Internal_Draw$length(summary.y))),
											_1: {ctor: '[]'}
										}
									}
								}
							},
							{ctor: '[]'}),
						_1: {ctor: '[]'}
					}),
				_1: customizations.defs
			});
	});
var _terezka$elm_plot$Plot$addNiceReachForBars = function (_p54) {
	var _p55 = _p54;
	var _p57 = _p55.y;
	var _p56 = _p55.x;
	return _elm_lang$core$Native_Utils.update(
		_p55,
		{
			x: _elm_lang$core$Native_Utils.update(
				_p56,
				{min: _p56.min - 0.5, max: _p56.max + 0.5}),
			y: _elm_lang$core$Native_Utils.update(
				_p57,
				{
					min: A2(_elm_lang$core$Basics$min, _p57.min, 0),
					max: _p57.max
				})
		});
};
var _terezka$elm_plot$Plot$addNiceReachForArea = F2(
	function (area, _p58) {
		var _p59 = _p58;
		var _p62 = _p59.y;
		var _p61 = _p59;
		var _p60 = area;
		if (_p60.ctor === 'Nothing') {
			return _p61;
		} else {
			return _elm_lang$core$Native_Utils.update(
				_p61,
				{
					x: _p59.x,
					y: _elm_lang$core$Native_Utils.update(
						_p62,
						{
							min: A2(_elm_lang$core$Basics$min, _p62.min, 0),
							max: _p62.max
						})
				});
		}
	});
var _terezka$elm_plot$Plot$addNiceReachForSeries = function (series) {
	var _p63 = series.interpolation;
	switch (_p63.ctor) {
		case 'None':
			return _elm_lang$core$Basics$identity;
		case 'Linear':
			return _terezka$elm_plot$Plot$addNiceReachForArea(_p63._0);
		default:
			return _terezka$elm_plot$Plot$addNiceReachForArea(_p63._0);
	}
};
var _terezka$elm_plot$Plot$viewSeriesCustom = F3(
	function (customizations, series, data) {
		var addNiceReach = function (summary) {
			return A3(_elm_lang$core$List$foldl, _terezka$elm_plot$Plot$addNiceReachForSeries, summary, series);
		};
		var dataPoints = A2(
			_elm_lang$core$List$map,
			function (_p64) {
				var _p65 = _p64;
				return _p65.toDataPoints(data);
			},
			series);
		var allDataPoints = _elm_lang$core$List$concat(dataPoints);
		var summary = A3(_terezka$elm_plot$Plot$toPlotSummary, customizations, addNiceReach, allDataPoints);
		var viewJunks = _elm_lang$core$Maybe$Just(
			A2(
				_elm_lang$svg$Svg$g,
				{
					ctor: '::',
					_0: _elm_lang$svg$Svg_Attributes$class('elm-plot__junk'),
					_1: {ctor: '[]'}
				},
				A2(
					_elm_lang$core$List$map,
					_terezka$elm_plot$Plot$viewActualJunk(summary),
					customizations.junk(summary))));
		var viewHorizontalAxes = A4(
			_terezka$elm_plot$Plot$viewHorizontalAxis,
			summary,
			customizations.horizontalAxis,
			{ctor: '[]'},
			A2(
				_elm_lang$core$List$filterMap,
				function (_) {
					return _.xTick;
				},
				allDataPoints));
		var viewGlitter = _elm_lang$core$Maybe$Just(
			A2(
				_elm_lang$svg$Svg$map,
				_elm_lang$core$Basics$never,
				A2(
					_elm_lang$svg$Svg$g,
					{
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$class('elm-plot__glitter'),
						_1: {ctor: '[]'}
					},
					A2(
						_elm_lang$core$List$concatMap,
						_terezka$elm_plot$Plot$viewGlitterLines(summary),
						allDataPoints))));
		var viewHint = function () {
			var _p66 = A2(
				_elm_lang$core$List$filterMap,
				function (_) {
					return _.hint;
				},
				allDataPoints);
			if (_p66.ctor === '[]') {
				return _elm_lang$svg$Svg$text('');
			} else {
				return A2(
					_elm_lang$html$Html$map,
					_elm_lang$core$Basics$never,
					A2(customizations.hintContainer, summary, _p66));
			}
		}();
		var viewVerticalAxes = _elm_lang$core$Maybe$Just(
			A2(
				_elm_lang$svg$Svg$g,
				{
					ctor: '::',
					_0: _elm_lang$svg$Svg_Attributes$class('elm-plot__vertical-axes'),
					_1: {ctor: '[]'}
				},
				A2(
					_elm_lang$core$List$filterMap,
					_elm_lang$core$Basics$identity,
					A3(
						_elm_lang$core$List$map2,
						function (_p67) {
							return A2(
								_terezka$elm_plot$Plot$viewVerticalAxis,
								summary,
								function (_) {
									return _.axis;
								}(_p67));
						},
						series,
						A2(
							_elm_lang$core$List$map,
							_elm_lang$core$List$filterMap(
								function (_) {
									return _.yTick;
								}),
							dataPoints)))));
		var viewActualSeries = _elm_lang$core$Maybe$Just(
			A2(
				_elm_lang$svg$Svg$g,
				{
					ctor: '::',
					_0: _elm_lang$svg$Svg_Attributes$class('elm-plot__all-series'),
					_1: {ctor: '[]'}
				},
				A3(
					_elm_lang$core$List$map2,
					A2(_terezka$elm_plot$Plot$viewASeries, customizations, summary),
					series,
					dataPoints)));
		var children = A2(
			_elm_lang$core$List$filterMap,
			_elm_lang$core$Basics$identity,
			{
				ctor: '::',
				_0: _elm_lang$core$Maybe$Just(
					A2(_terezka$elm_plot$Plot$defineClipPath, customizations, summary)),
				_1: {
					ctor: '::',
					_0: A2(_terezka$elm_plot$Plot$viewHorizontalGrid, summary, customizations.grid.horizontal),
					_1: {
						ctor: '::',
						_0: A2(_terezka$elm_plot$Plot$viewVerticalGrid, summary, customizations.grid.vertical),
						_1: {
							ctor: '::',
							_0: viewActualSeries,
							_1: {
								ctor: '::',
								_0: viewHorizontalAxes,
								_1: {
									ctor: '::',
									_0: viewVerticalAxes,
									_1: {
										ctor: '::',
										_0: viewGlitter,
										_1: {
											ctor: '::',
											_0: viewJunks,
											_1: {ctor: '[]'}
										}
									}
								}
							}
						}
					}
				}
			});
		return A2(
			_elm_lang$html$Html$div,
			A2(_terezka$elm_plot$Plot$containerAttributes, customizations, summary),
			{
				ctor: '::',
				_0: A2(
					_elm_lang$svg$Svg$svg,
					_terezka$elm_plot$Plot$innerAttributes(customizations),
					children),
				_1: {
					ctor: '::',
					_0: viewHint,
					_1: {ctor: '[]'}
				}
			});
	});
var _terezka$elm_plot$Plot$closestToZero = F2(
	function (min, max) {
		return A3(_elm_lang$core$Basics$clamp, min, max, 0);
	});
var _terezka$elm_plot$Plot$viewActualBars = F3(
	function (summary, _p68, groups) {
		var _p69 = _p68;
		var _p75 = _p69.styles;
		var indexedHeights = function (group) {
			return A2(
				_elm_lang$core$List$indexedMap,
				F2(
					function (v0, v1) {
						return {ctor: '_Tuple2', _0: v0, _1: v1};
					}),
				group.bars);
		};
		var barsPerGroup = _elm_lang$core$Basics$toFloat(
			_elm_lang$core$List$length(_p75));
		var defaultWidth = 1 / barsPerGroup;
		var width = function () {
			var _p70 = _p69.maxWidth;
			if (_p70.ctor === 'Percentage') {
				return (defaultWidth * _elm_lang$core$Basics$toFloat(_p70._0)) / 100;
			} else {
				var _p71 = _p70._0;
				return (_elm_lang$core$Native_Utils.cmp(
					defaultWidth,
					A2(_terezka$elm_plot$Internal_Draw$unScaleValue, summary.x, _p71)) > 0) ? A2(_terezka$elm_plot$Internal_Draw$unScaleValue, summary.x, _p71) : defaultWidth;
			}
		}();
		var viewLabel = function (label) {
			return A2(
				_elm_lang$svg$Svg$g,
				{
					ctor: '::',
					_0: _elm_lang$svg$Svg_Attributes$transform(
						A2(
							_elm_lang$core$Basics_ops['++'],
							'translate(',
							A2(
								_elm_lang$core$Basics_ops['++'],
								_elm_lang$core$Basics$toString(
									A2(_terezka$elm_plot$Internal_Draw$scaleValue, summary.x, width / 2)),
								', -5)'))),
					_1: {
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$style('text-anchor: middle;'),
						_1: {ctor: '[]'}
					}
				},
				{
					ctor: '::',
					_0: label,
					_1: {ctor: '[]'}
				});
		};
		var offset = F2(
			function (x, i) {
				return x + (width * (_elm_lang$core$Basics$toFloat(i) - (barsPerGroup / 2)));
			});
		var viewBar = F3(
			function (x, attributes, _p72) {
				var _p73 = _p72;
				var _p74 = _p73._1.height;
				return A2(
					_elm_lang$svg$Svg$g,
					{
						ctor: '::',
						_0: A4(
							_terezka$elm_plot$Internal_Draw$place,
							summary,
							{
								x: A2(offset, x, _p73._0),
								y: A2(
									_elm_lang$core$Basics$max,
									A2(_terezka$elm_plot$Plot$closestToZero, summary.y.min, summary.y.max),
									_p74)
							},
							0,
							0),
						_1: {ctor: '[]'}
					},
					{
						ctor: '::',
						_0: A2(
							_elm_lang$svg$Svg$map,
							_elm_lang$core$Basics$never,
							A2(
								_elm_lang$core$Maybe$withDefault,
								_elm_lang$svg$Svg$text(''),
								A2(_elm_lang$core$Maybe$map, viewLabel, _p73._1.label))),
						_1: {
							ctor: '::',
							_0: A2(
								_elm_lang$svg$Svg$rect,
								A2(
									_elm_lang$core$Basics_ops['++'],
									attributes,
									{
										ctor: '::',
										_0: _elm_lang$svg$Svg_Attributes$width(
											_elm_lang$core$Basics$toString(
												A2(_terezka$elm_plot$Internal_Draw$scaleValue, summary.x, width))),
										_1: {
											ctor: '::',
											_0: _elm_lang$svg$Svg_Attributes$height(
												_elm_lang$core$Basics$toString(
													A2(
														_terezka$elm_plot$Internal_Draw$scaleValue,
														summary.y,
														_elm_lang$core$Basics$abs(_p74)))),
											_1: {ctor: '[]'}
										}
									}),
								{ctor: '[]'}),
							_1: {ctor: '[]'}
						}
					});
			});
		var viewGroup = F2(
			function (index, group) {
				return A2(
					_elm_lang$svg$Svg$g,
					{
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$class('elm-plot__bars__group'),
						_1: {ctor: '[]'}
					},
					A3(
						_elm_lang$core$List$map2,
						viewBar(
							_elm_lang$core$Basics$toFloat(index + 1)),
						_p75,
						indexedHeights(group)));
			});
		return _elm_lang$core$Maybe$Just(
			A2(
				_elm_lang$svg$Svg$g,
				{
					ctor: '::',
					_0: _elm_lang$svg$Svg_Attributes$class('elm-plot__bars'),
					_1: {ctor: '[]'}
				},
				A2(_elm_lang$core$List$indexedMap, viewGroup, groups)));
	});
var _terezka$elm_plot$Plot$viewBarsCustom = F3(
	function (customizations, bars, data) {
		var toDataPoint = F3(
			function (index, group, _p76) {
				var _p77 = _p76;
				return {
					x: _elm_lang$core$Basics$toFloat(index) + 1,
					y: _p77.height,
					xLine: group.verticalLine(
						_elm_lang$core$Basics$toFloat(index) + 1),
					yLine: _elm_lang$core$Maybe$Nothing
				};
			});
		var toDataPoints = F2(
			function (index, group) {
				return A2(
					_elm_lang$core$List$map,
					A2(toDataPoint, index, group),
					group.bars);
			});
		var groups = bars.toGroups(data);
		var dataPoints = _elm_lang$core$List$concat(
			A2(_elm_lang$core$List$indexedMap, toDataPoints, groups));
		var summary = A3(_terezka$elm_plot$Plot$toPlotSummary, customizations, _terezka$elm_plot$Plot$addNiceReachForBars, dataPoints);
		var viewGlitter = _elm_lang$core$Maybe$Just(
			A2(
				_elm_lang$svg$Svg$map,
				_elm_lang$core$Basics$never,
				A2(
					_elm_lang$svg$Svg$g,
					{
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$class('elm-plot__glitter'),
						_1: {ctor: '[]'}
					},
					A2(
						_elm_lang$core$List$concatMap,
						_terezka$elm_plot$Plot$viewGlitterLines(summary),
						dataPoints))));
		var xLabels = A2(
			_elm_lang$core$List$indexedMap,
			F2(
				function (index, group) {
					return group.label(
						_elm_lang$core$Basics$toFloat(index) + 1);
				}),
			groups);
		var hints = A2(
			_elm_lang$core$List$filterMap,
			_elm_lang$core$Basics$identity,
			A2(
				_elm_lang$core$List$indexedMap,
				F2(
					function (index, group) {
						return group.hint(
							_elm_lang$core$Basics$toFloat(index) + 1);
					}),
				groups));
		var viewHint = function () {
			var _p78 = hints;
			if (_p78.ctor === '[]') {
				return _elm_lang$svg$Svg$text('');
			} else {
				return A2(
					_elm_lang$html$Html$map,
					_elm_lang$core$Basics$never,
					A2(customizations.hintContainer, summary, _p78));
			}
		}();
		var children = A2(
			_elm_lang$core$List$filterMap,
			_elm_lang$core$Basics$identity,
			{
				ctor: '::',
				_0: _elm_lang$core$Maybe$Just(
					A2(_terezka$elm_plot$Plot$defineClipPath, customizations, summary)),
				_1: {
					ctor: '::',
					_0: A2(_terezka$elm_plot$Plot$viewHorizontalGrid, summary, customizations.grid.horizontal),
					_1: {
						ctor: '::',
						_0: A2(_terezka$elm_plot$Plot$viewVerticalGrid, summary, customizations.grid.vertical),
						_1: {
							ctor: '::',
							_0: A3(_terezka$elm_plot$Plot$viewActualBars, summary, bars, groups),
							_1: {
								ctor: '::',
								_0: A4(
									_terezka$elm_plot$Plot$viewHorizontalAxis,
									summary,
									customizations.horizontalAxis,
									xLabels,
									{ctor: '[]'}),
								_1: {
									ctor: '::',
									_0: A3(
										_terezka$elm_plot$Plot$viewVerticalAxis,
										summary,
										bars.axis,
										{ctor: '[]'}),
									_1: {
										ctor: '::',
										_0: viewGlitter,
										_1: {ctor: '[]'}
									}
								}
							}
						}
					}
				}
			});
		return A2(
			_elm_lang$html$Html$div,
			A2(_terezka$elm_plot$Plot$containerAttributes, customizations, summary),
			{
				ctor: '::',
				_0: A2(
					_elm_lang$svg$Svg$svg,
					_terezka$elm_plot$Plot$innerAttributes(customizations),
					children),
				_1: {
					ctor: '::',
					_0: viewHint,
					_1: {ctor: '[]'}
				}
			});
	});
var _terezka$elm_plot$Plot$displace = F2(
	function (x, y) {
		return _elm_lang$svg$Svg_Attributes$transform(
			A2(
				_elm_lang$core$Basics_ops['++'],
				'translate(',
				A2(
					_elm_lang$core$Basics_ops['++'],
					_elm_lang$core$Basics$toString(x),
					A2(
						_elm_lang$core$Basics_ops['++'],
						', ',
						A2(
							_elm_lang$core$Basics_ops['++'],
							_elm_lang$core$Basics$toString(y),
							')')))));
	});
var _terezka$elm_plot$Plot$fullLine = F2(
	function (attributes, summary) {
		return {
			attributes: {
				ctor: '::',
				_0: _elm_lang$svg$Svg_Attributes$style('pointer-events: none;'),
				_1: attributes
			},
			start: summary.min,
			end: summary.max
		};
	});
var _terezka$elm_plot$Plot$simpleLabel = function (position) {
	return {
		position: position,
		view: A2(
			_terezka$elm_plot$Plot$viewLabel,
			{ctor: '[]'},
			_elm_lang$core$Basics$toString(position))
	};
};
var _terezka$elm_plot$Plot$simpleTick = function (position) {
	return {
		position: position,
		length: 5,
		attributes: {
			ctor: '::',
			_0: _elm_lang$svg$Svg_Attributes$stroke(_terezka$elm_plot$Internal_Colors$darkGrey),
			_1: {ctor: '[]'}
		}
	};
};
var _terezka$elm_plot$Plot$simpleLine = function (summary) {
	return A2(
		_terezka$elm_plot$Plot$fullLine,
		{
			ctor: '::',
			_0: _elm_lang$svg$Svg_Attributes$stroke(_terezka$elm_plot$Internal_Colors$darkGrey),
			_1: {ctor: '[]'}
		},
		summary);
};
var _terezka$elm_plot$Plot$normalHintContainerInner = F2(
	function (isLeft, hints) {
		var margin = isLeft ? 10 : 10;
		return A2(
			_elm_lang$html$Html$div,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$style(
					{
						ctor: '::',
						_0: {
							ctor: '_Tuple2',
							_0: 'margin',
							_1: A2(
								_elm_lang$core$Basics_ops['++'],
								'0 ',
								A2(
									_elm_lang$core$Basics_ops['++'],
									_elm_lang$core$Basics$toString(margin),
									'px'))
						},
						_1: {
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'padding', _1: '5px 10px'},
							_1: {
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: 'background', _1: _terezka$elm_plot$Internal_Colors$grey},
								_1: {
									ctor: '::',
									_0: {ctor: '_Tuple2', _0: 'border-radius', _1: '2px'},
									_1: {
										ctor: '::',
										_0: {ctor: '_Tuple2', _0: 'color', _1: 'black'},
										_1: {ctor: '[]'}
									}
								}
							}
						}
					}),
				_1: {
					ctor: '::',
					_0: _elm_lang$svg$Svg_Attributes$class('elm-plot__hint'),
					_1: {ctor: '[]'}
				}
			},
			hints);
	});
var _terezka$elm_plot$Plot$viewFlyingHintContainer = F4(
	function (inner, _p79, summary, hints) {
		var _p80 = _p79;
		var _p81 = _p80.x;
		var isLeft = _elm_lang$core$Native_Utils.cmp(
			_p81 - summary.x.min,
			_terezka$elm_plot$Internal_Draw$range(summary.x) / 2) > 0;
		var direction = isLeft ? 'translateX(-100%)' : 'translateX(0)';
		var xOffset = (A2(_terezka$elm_plot$Internal_Draw$toSVGX, summary, _p81) * 100) / summary.x.length;
		var style = {
			ctor: '::',
			_0: {ctor: '_Tuple2', _0: 'position', _1: 'absolute'},
			_1: {
				ctor: '::',
				_0: {ctor: '_Tuple2', _0: 'top', _1: '25%'},
				_1: {
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'left',
						_1: A2(
							_elm_lang$core$Basics_ops['++'],
							_elm_lang$core$Basics$toString(xOffset),
							'%')
					},
					_1: {
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: 'transform', _1: direction},
						_1: {
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'pointer-events', _1: 'none'},
							_1: {ctor: '[]'}
						}
					}
				}
			}
		};
		return A2(
			_elm_lang$html$Html$div,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$style(style),
				_1: {
					ctor: '::',
					_0: _elm_lang$svg$Svg_Attributes$class('elm-plot__hint'),
					_1: {ctor: '[]'}
				}
			},
			{
				ctor: '::',
				_0: A2(inner, isLeft, hints),
				_1: {ctor: '[]'}
			});
	});
var _terezka$elm_plot$Plot$flyingHintContainer = F4(
	function (inner, hovering, summary, hints) {
		var _p82 = hovering;
		if (_p82.ctor === 'Nothing') {
			return _elm_lang$svg$Svg$text('');
		} else {
			return A4(_terezka$elm_plot$Plot$viewFlyingHintContainer, inner, _p82._0, summary, hints);
		}
	});
var _terezka$elm_plot$Plot$normalHintContainer = function (summary) {
	return _elm_lang$html$Html$div(
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$style(
				{
					ctor: '::',
					_0: {
						ctor: '_Tuple2',
						_0: 'margin-left',
						_1: A2(
							_elm_lang$core$Basics_ops['++'],
							_elm_lang$core$Basics$toString(summary.x.marginLower),
							'px')
					},
					_1: {ctor: '[]'}
				}),
			_1: {ctor: '[]'}
		});
};
var _terezka$elm_plot$Plot$junk = F3(
	function (title, x, y) {
		return {x: x, y: y, view: title};
	});
var _terezka$elm_plot$Plot$normalBarLabel = F2(
	function (label, position) {
		return {
			position: position,
			view: A2(
				_terezka$elm_plot$Plot$viewLabel,
				{ctor: '[]'},
				label)
		};
	});
var _terezka$elm_plot$Plot$normalHint = function (y) {
	return A2(
		_elm_lang$html$Html$span,
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$style(
				{
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: 'padding', _1: '5px'},
					_1: {ctor: '[]'}
				}),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: _elm_lang$html$Html$text(
				A2(
					_elm_lang$core$Basics_ops['++'],
					'y: ',
					_elm_lang$core$Basics$toString(y))),
			_1: {ctor: '[]'}
		});
};
var _terezka$elm_plot$Plot$rangeFrameDot = F3(
	function (view, x, y) {
		return {
			view: _elm_lang$core$Maybe$Just(view),
			xLine: _elm_lang$core$Maybe$Nothing,
			yLine: _elm_lang$core$Maybe$Nothing,
			xTick: _elm_lang$core$Maybe$Just(
				_terezka$elm_plot$Plot$simpleTick(x)),
			yTick: _elm_lang$core$Maybe$Just(
				_terezka$elm_plot$Plot$simpleTick(y)),
			hint: _elm_lang$core$Maybe$Nothing,
			x: x,
			y: y
		};
	});
var _terezka$elm_plot$Plot$emphasizedDot = F3(
	function (view, x, y) {
		return {
			view: _elm_lang$core$Maybe$Just(view),
			xLine: _elm_lang$core$Maybe$Just(
				_terezka$elm_plot$Plot$fullLine(
					{
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$stroke(_terezka$elm_plot$Internal_Colors$darkGrey),
						_1: {
							ctor: '::',
							_0: _elm_lang$svg$Svg_Attributes$strokeDasharray('5, 5'),
							_1: {ctor: '[]'}
						}
					})),
			yLine: _elm_lang$core$Maybe$Just(
				_terezka$elm_plot$Plot$fullLine(
					{
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$stroke(_terezka$elm_plot$Internal_Colors$darkGrey),
						_1: {
							ctor: '::',
							_0: _elm_lang$svg$Svg_Attributes$strokeDasharray('5, 5'),
							_1: {ctor: '[]'}
						}
					})),
			xTick: _elm_lang$core$Maybe$Nothing,
			yTick: _elm_lang$core$Maybe$Nothing,
			hint: _elm_lang$core$Maybe$Nothing,
			x: x,
			y: y
		};
	});
var _terezka$elm_plot$Plot$onHovering = F3(
	function (stuff, hovering, x) {
		return A2(
			_elm_lang$core$Maybe$andThen,
			function (p) {
				return _elm_lang$core$Native_Utils.eq(p.x, x) ? _elm_lang$core$Maybe$Just(stuff) : _elm_lang$core$Maybe$Nothing;
			},
			hovering);
	});
var _terezka$elm_plot$Plot$hintDot = F4(
	function (view, hovering, x, y) {
		return {
			view: _elm_lang$core$Maybe$Just(view),
			xLine: A3(
				_terezka$elm_plot$Plot$onHovering,
				_terezka$elm_plot$Plot$fullLine(
					{
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$stroke(_terezka$elm_plot$Internal_Colors$darkGrey),
						_1: {ctor: '[]'}
					}),
				hovering,
				x),
			yLine: _elm_lang$core$Maybe$Nothing,
			xTick: _elm_lang$core$Maybe$Nothing,
			yTick: _elm_lang$core$Maybe$Nothing,
			hint: A3(
				_terezka$elm_plot$Plot$onHovering,
				_terezka$elm_plot$Plot$normalHint(y),
				hovering,
				x),
			x: x,
			y: y
		};
	});
var _terezka$elm_plot$Plot$dot = F3(
	function (view, x, y) {
		return {
			view: _elm_lang$core$Maybe$Just(view),
			xLine: _elm_lang$core$Maybe$Nothing,
			yLine: _elm_lang$core$Maybe$Nothing,
			xTick: _elm_lang$core$Maybe$Nothing,
			yTick: _elm_lang$core$Maybe$Nothing,
			hint: _elm_lang$core$Maybe$Nothing,
			x: x,
			y: y
		};
	});
var _terezka$elm_plot$Plot$clear = _terezka$elm_plot$Plot$dot(
	_elm_lang$svg$Svg$text(''));
var _terezka$elm_plot$Plot$triangle = _terezka$elm_plot$Plot$dot(
	_terezka$elm_plot$Plot$viewTriangle(_terezka$elm_plot$Internal_Colors$pinkStroke));
var _terezka$elm_plot$Plot$diamond = _terezka$elm_plot$Plot$dot(
	A3(_terezka$elm_plot$Plot$viewDiamond, 10, 10, _terezka$elm_plot$Internal_Colors$pinkStroke));
var _terezka$elm_plot$Plot$square = _terezka$elm_plot$Plot$dot(
	A2(_terezka$elm_plot$Plot$viewSquare, 10, _terezka$elm_plot$Internal_Colors$pinkStroke));
var _terezka$elm_plot$Plot$circle = _terezka$elm_plot$Plot$dot(
	A2(_terezka$elm_plot$Plot$viewCircle, 5, _terezka$elm_plot$Internal_Colors$pinkStroke));
var _terezka$elm_plot$Plot$PlotSummary = F2(
	function (a, b) {
		return {x: a, y: b};
	});
var _terezka$elm_plot$Plot$AxisSummary = F8(
	function (a, b, c, d, e, f, g, h) {
		return {min: a, max: b, dataMin: c, dataMax: d, marginLower: e, marginUpper: f, length: g, all: h};
	});
var _terezka$elm_plot$Plot$DataPoint = F8(
	function (a, b, c, d, e, f, g, h) {
		return {view: a, xLine: b, yLine: c, xTick: d, yTick: e, hint: f, x: g, y: h};
	});
var _terezka$elm_plot$Plot$customDot = _terezka$elm_plot$Plot$DataPoint;
var _terezka$elm_plot$Plot$Series = F3(
	function (a, b, c) {
		return {axis: a, interpolation: b, toDataPoints: c};
	});
var _terezka$elm_plot$Plot$customSeries = _terezka$elm_plot$Plot$Series;
var _terezka$elm_plot$Plot$Bars = F4(
	function (a, b, c, d) {
		return {axis: a, toGroups: b, styles: c, maxWidth: d};
	});
var _terezka$elm_plot$Plot$customGroups = _terezka$elm_plot$Plot$Bars;
var _terezka$elm_plot$Plot$BarGroup = F4(
	function (a, b, c, d) {
		return {label: a, hint: b, verticalLine: c, bars: d};
	});
var _terezka$elm_plot$Plot$customGroup = _terezka$elm_plot$Plot$BarGroup;
var _terezka$elm_plot$Plot$Bar = F2(
	function (a, b) {
		return {label: a, height: b};
	});
var _terezka$elm_plot$Plot$group = F2(
	function (label, heights) {
		return {
			label: _terezka$elm_plot$Plot$normalBarLabel(label),
			verticalLine: _elm_lang$core$Basics$always(_elm_lang$core$Maybe$Nothing),
			hint: _elm_lang$core$Basics$always(_elm_lang$core$Maybe$Nothing),
			bars: A2(
				_elm_lang$core$List$map,
				_terezka$elm_plot$Plot$Bar(_elm_lang$core$Maybe$Nothing),
				heights)
		};
	});
var _terezka$elm_plot$Plot$hintGroup = F3(
	function (hovering, label, heights) {
		return {
			label: _terezka$elm_plot$Plot$normalBarLabel(label),
			verticalLine: A2(
				_terezka$elm_plot$Plot$onHovering,
				_terezka$elm_plot$Plot$fullLine(
					{
						ctor: '::',
						_0: _elm_lang$svg$Svg_Attributes$stroke(_terezka$elm_plot$Internal_Colors$darkGrey),
						_1: {ctor: '[]'}
					}),
				hovering),
			hint: function (g) {
				return A3(
					_terezka$elm_plot$Plot$onHovering,
					A2(
						_elm_lang$html$Html$div,
						{ctor: '[]'},
						A2(_elm_lang$core$List$map, _terezka$elm_plot$Plot$normalHint, heights)),
					hovering,
					g);
			},
			bars: A2(
				_elm_lang$core$List$map,
				_terezka$elm_plot$Plot$Bar(_elm_lang$core$Maybe$Nothing),
				heights)
		};
	});
var _terezka$elm_plot$Plot$histogramBar = function (height) {
	return {
		label: _terezka$elm_plot$Plot$simpleLabel,
		verticalLine: _elm_lang$core$Basics$always(_elm_lang$core$Maybe$Nothing),
		hint: _elm_lang$core$Basics$always(_elm_lang$core$Maybe$Nothing),
		bars: {
			ctor: '::',
			_0: A2(_terezka$elm_plot$Plot$Bar, _elm_lang$core$Maybe$Nothing, height),
			_1: {ctor: '[]'}
		}
	};
};
var _terezka$elm_plot$Plot$PlotCustomizations = function (a) {
	return function (b) {
		return function (c) {
			return function (d) {
				return function (e) {
					return function (f) {
						return function (g) {
							return function (h) {
								return function (i) {
									return function (j) {
										return function (k) {
											return function (l) {
												return function (m) {
													return function (n) {
														return function (o) {
															return {attributes: a, id: b, width: c, height: d, defs: e, margin: f, onHover: g, hintContainer: h, horizontalAxis: i, grid: j, junk: k, toDomainLowest: l, toDomainHighest: m, toRangeLowest: n, toRangeHighest: o};
														};
													};
												};
											};
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var _terezka$elm_plot$Plot$JunkCustomizations = F3(
	function (a, b, c) {
		return {x: a, y: b, view: c};
	});
var _terezka$elm_plot$Plot$GridLineCustomizations = F2(
	function (a, b) {
		return {attributes: a, position: b};
	});
var _terezka$elm_plot$Plot$AxisCustomizations = F5(
	function (a, b, c, d, e) {
		return {position: a, axisLine: b, ticks: c, labels: d, flipAnchor: e};
	});
var _terezka$elm_plot$Plot$LineCustomizations = F3(
	function (a, b, c) {
		return {attributes: a, start: b, end: c};
	});
var _terezka$elm_plot$Plot$TickCustomizations = F3(
	function (a, b, c) {
		return {attributes: a, length: b, position: c};
	});
var _terezka$elm_plot$Plot$LabelCustomizations = F2(
	function (a, b) {
		return {view: a, position: b};
	});
var _terezka$elm_plot$Plot$TempPlotSummary = F2(
	function (a, b) {
		return {x: a, y: b};
	});
var _terezka$elm_plot$Plot$Monotone = F2(
	function (a, b) {
		return {ctor: 'Monotone', _0: a, _1: b};
	});
var _terezka$elm_plot$Plot$Linear = F2(
	function (a, b) {
		return {ctor: 'Linear', _0: a, _1: b};
	});
var _terezka$elm_plot$Plot$None = {ctor: 'None'};
var _terezka$elm_plot$Plot$Fixed = function (a) {
	return {ctor: 'Fixed', _0: a};
};
var _terezka$elm_plot$Plot$Percentage = function (a) {
	return {ctor: 'Percentage', _0: a};
};
var _terezka$elm_plot$Plot$YeahGridsAreTotallyLame = {ctor: 'YeahGridsAreTotallyLame'};
var _terezka$elm_plot$Plot$clearGrid = _terezka$elm_plot$Plot$YeahGridsAreTotallyLame;
var _terezka$elm_plot$Plot$Grid = function (a) {
	return {ctor: 'Grid', _0: a};
};
var _terezka$elm_plot$Plot$customGrid = _terezka$elm_plot$Plot$Grid;
var _terezka$elm_plot$Plot$decentGrid = _terezka$elm_plot$Plot$customGrid(
	function (summary) {
		return A2(
			_elm_lang$core$List$map,
			_terezka$elm_plot$Plot$GridLineCustomizations(
				{
					ctor: '::',
					_0: _elm_lang$svg$Svg_Attributes$stroke(_terezka$elm_plot$Internal_Colors$grey),
					_1: {ctor: '[]'}
				}),
			_terezka$elm_plot$Plot$decentPositions(summary));
	});
var _terezka$elm_plot$Plot$SometimesYouDoNotHaveAnAxis = {ctor: 'SometimesYouDoNotHaveAnAxis'};
var _terezka$elm_plot$Plot$sometimesYouDoNotHaveAnAxis = _terezka$elm_plot$Plot$SometimesYouDoNotHaveAnAxis;
var _terezka$elm_plot$Plot$Axis = function (a) {
	return {ctor: 'Axis', _0: a};
};
var _terezka$elm_plot$Plot$customAxis = _terezka$elm_plot$Plot$Axis;
var _terezka$elm_plot$Plot$normalBarsAxis = _terezka$elm_plot$Plot$customAxis(
	function (summary) {
		return {
			position: _terezka$elm_plot$Plot$closestToZero,
			axisLine: _elm_lang$core$Maybe$Just(
				_terezka$elm_plot$Plot$simpleLine(summary)),
			ticks: A2(
				_elm_lang$core$List$map,
				_terezka$elm_plot$Plot$simpleTick,
				A3(_terezka$elm_plot$Plot$interval, 0, 1, summary)),
			labels: {ctor: '[]'},
			flipAnchor: false
		};
	});
var _terezka$elm_plot$Plot$normalAxis = _terezka$elm_plot$Plot$customAxis(
	function (summary) {
		return {
			position: _terezka$elm_plot$Plot$closestToZero,
			axisLine: _elm_lang$core$Maybe$Just(
				_terezka$elm_plot$Plot$simpleLine(summary)),
			ticks: A2(
				_elm_lang$core$List$map,
				_terezka$elm_plot$Plot$simpleTick,
				A2(
					_terezka$elm_plot$Plot$remove,
					0,
					_terezka$elm_plot$Plot$decentPositions(summary))),
			labels: A2(
				_elm_lang$core$List$map,
				_terezka$elm_plot$Plot$simpleLabel,
				A2(
					_terezka$elm_plot$Plot$remove,
					0,
					_terezka$elm_plot$Plot$decentPositions(summary))),
			flipAnchor: false
		};
	});
var _terezka$elm_plot$Plot$dots = function (toDataPoints) {
	return {axis: _terezka$elm_plot$Plot$normalAxis, interpolation: _terezka$elm_plot$Plot$None, toDataPoints: toDataPoints};
};
var _terezka$elm_plot$Plot$line = function (toDataPoints) {
	return {
		axis: _terezka$elm_plot$Plot$normalAxis,
		interpolation: A2(
			_terezka$elm_plot$Plot$Linear,
			_elm_lang$core$Maybe$Nothing,
			{
				ctor: '::',
				_0: _elm_lang$svg$Svg_Attributes$stroke(_terezka$elm_plot$Internal_Colors$pinkStroke),
				_1: {ctor: '[]'}
			}),
		toDataPoints: toDataPoints
	};
};
var _terezka$elm_plot$Plot$area = function (toDataPoints) {
	return {
		axis: _terezka$elm_plot$Plot$normalAxis,
		interpolation: A2(
			_terezka$elm_plot$Plot$Linear,
			_elm_lang$core$Maybe$Just(_terezka$elm_plot$Internal_Colors$pinkFill),
			{
				ctor: '::',
				_0: _elm_lang$svg$Svg_Attributes$stroke(_terezka$elm_plot$Internal_Colors$pinkStroke),
				_1: {ctor: '[]'}
			}),
		toDataPoints: toDataPoints
	};
};
var _terezka$elm_plot$Plot$groups = function (toGroups) {
	return {
		axis: _terezka$elm_plot$Plot$normalAxis,
		toGroups: toGroups,
		styles: {
			ctor: '::',
			_0: {
				ctor: '::',
				_0: _elm_lang$svg$Svg_Attributes$fill(_terezka$elm_plot$Internal_Colors$pinkFill),
				_1: {ctor: '[]'}
			},
			_1: {
				ctor: '::',
				_0: {
					ctor: '::',
					_0: _elm_lang$svg$Svg_Attributes$fill(_terezka$elm_plot$Internal_Colors$blueFill),
					_1: {ctor: '[]'}
				},
				_1: {ctor: '[]'}
			}
		},
		maxWidth: _terezka$elm_plot$Plot$Percentage(75)
	};
};
var _terezka$elm_plot$Plot$histogram = function (toGroups) {
	return {
		axis: _terezka$elm_plot$Plot$normalAxis,
		toGroups: toGroups,
		styles: {
			ctor: '::',
			_0: {
				ctor: '::',
				_0: _elm_lang$svg$Svg_Attributes$fill(_terezka$elm_plot$Internal_Colors$pinkFill),
				_1: {
					ctor: '::',
					_0: _elm_lang$svg$Svg_Attributes$stroke(_terezka$elm_plot$Internal_Colors$pinkStroke),
					_1: {ctor: '[]'}
				}
			},
			_1: {ctor: '[]'}
		},
		maxWidth: _terezka$elm_plot$Plot$Percentage(100)
	};
};
var _terezka$elm_plot$Plot$defaultSeriesPlotCustomizations = {
	attributes: {ctor: '[]'},
	defs: {ctor: '[]'},
	id: 'elm-plot',
	width: 647,
	height: 440,
	margin: {top: 20, right: 40, bottom: 20, left: 40},
	onHover: _elm_lang$core$Maybe$Nothing,
	hintContainer: _terezka$elm_plot$Plot$normalHintContainer,
	horizontalAxis: _terezka$elm_plot$Plot$normalAxis,
	grid: {horizontal: _terezka$elm_plot$Plot$clearGrid, vertical: _terezka$elm_plot$Plot$clearGrid},
	junk: _elm_lang$core$Basics$always(
		{ctor: '[]'}),
	toDomainLowest: _elm_lang$core$Basics$identity,
	toDomainHighest: _elm_lang$core$Basics$identity,
	toRangeLowest: _elm_lang$core$Basics$identity,
	toRangeHighest: _elm_lang$core$Basics$identity
};
var _terezka$elm_plot$Plot$defaultBarsPlotCustomizations = _elm_lang$core$Native_Utils.update(
	_terezka$elm_plot$Plot$defaultSeriesPlotCustomizations,
	{
		horizontalAxis: _terezka$elm_plot$Plot$normalBarsAxis,
		margin: {top: 20, right: 40, bottom: 40, left: 40}
	});
var _terezka$elm_plot$Plot$viewBars = _terezka$elm_plot$Plot$viewBarsCustom(_terezka$elm_plot$Plot$defaultBarsPlotCustomizations);
var _terezka$elm_plot$Plot$viewSeries = _terezka$elm_plot$Plot$viewSeriesCustom(_terezka$elm_plot$Plot$defaultSeriesPlotCustomizations);
var _terezka$elm_plot$Plot$clearAxis = _terezka$elm_plot$Plot$customAxis(
	function (summary) {
		return {
			position: _terezka$elm_plot$Plot$closestToZero,
			axisLine: _elm_lang$core$Maybe$Nothing,
			ticks: {ctor: '[]'},
			labels: {ctor: '[]'},
			flipAnchor: false
		};
	});
var _terezka$elm_plot$Plot$axisAtMin = _terezka$elm_plot$Plot$customAxis(
	function (summary) {
		return {
			position: _elm_lang$core$Basics$min,
			axisLine: _elm_lang$core$Maybe$Just(
				_terezka$elm_plot$Plot$simpleLine(summary)),
			ticks: A2(
				_elm_lang$core$List$map,
				_terezka$elm_plot$Plot$simpleTick,
				_terezka$elm_plot$Plot$decentPositions(summary)),
			labels: A2(
				_elm_lang$core$List$map,
				_terezka$elm_plot$Plot$simpleLabel,
				_terezka$elm_plot$Plot$decentPositions(summary)),
			flipAnchor: false
		};
	});
var _terezka$elm_plot$Plot$axisAtMax = _terezka$elm_plot$Plot$customAxis(
	function (summary) {
		return {
			position: _elm_lang$core$Basics$max,
			axisLine: _elm_lang$core$Maybe$Just(
				_terezka$elm_plot$Plot$simpleLine(summary)),
			ticks: A2(
				_elm_lang$core$List$map,
				_terezka$elm_plot$Plot$simpleTick,
				_terezka$elm_plot$Plot$decentPositions(summary)),
			labels: A2(
				_elm_lang$core$List$map,
				_terezka$elm_plot$Plot$simpleLabel,
				_terezka$elm_plot$Plot$decentPositions(summary)),
			flipAnchor: true
		};
	});

var _user$project$Crd$asDataIn = F2(
	function (model, data_array) {
		return _elm_lang$core$Native_Utils.update(
			model,
			{data: data_array});
	});
var _user$project$Crd$asCvtIn = F2(
	function (model, cvt) {
		return _elm_lang$core$Native_Utils.update(
			model,
			{cvt: cvt});
	});
var _user$project$Crd$setHeaterPID = F2(
	function (pid, htr) {
		return _elm_lang$core$Native_Utils.update(
			htr,
			{pid: pid});
	});
var _user$project$Crd$setHeaterSP = F2(
	function (sp, htr) {
		return _elm_lang$core$Native_Utils.update(
			htr,
			{sp: sp});
	});
var _user$project$Crd$togglePower = function (cvt) {
	return _elm_lang$core$Native_Utils.update(
		cvt,
		{power: !cvt.power});
};
var _user$project$Crd$setDc = F2(
	function (dc, cvt) {
		return _elm_lang$core$Native_Utils.update(
			cvt,
			{dc: dc});
	});
var _user$project$Crd$setRate = F2(
	function (f, cvt) {
		return _elm_lang$core$Native_Utils.update(
			cvt,
			{rate: f});
	});
var _user$project$Crd$update = F2(
	function (msg, model) {
		var _p0 = msg;
		switch (_p0.ctor) {
			case 'UpdateFrequency':
				var f = A2(
					_elm_lang$core$Result$withDefault,
					1000,
					_elm_lang$core$String$toInt(_p0._0));
				var new_model = A2(
					_user$project$Crd$asCvtIn,
					model,
					A2(_user$project$Crd$setRate, f, model.cvt));
				return new_model;
			case 'UpdateDutyCycle':
				var dc_ = A2(
					_elm_lang$core$Debug$log,
					'dc',
					A2(
						_elm_lang$core$Result$withDefault,
						0,
						_elm_lang$core$String$toInt(_p0._0)));
				var new_model = A2(
					_user$project$Crd$asCvtIn,
					model,
					A2(_user$project$Crd$setDc, dc_, model.cvt));
				return new_model;
			default:
				return A2(
					_user$project$Crd$asCvtIn,
					model,
					_user$project$Crd$togglePower(model.cvt));
		}
	});
var _user$project$Crd$Model = F2(
	function (a, b) {
		return {cvt: a, data: b};
	});
var _user$project$Crd$CrdsCell = function (a) {
	return function (b) {
		return function (c) {
			return function (d) {
				return function (e) {
					return function (f) {
						return function (g) {
							return function (h) {
								return function (i) {
									return function (j) {
										return {tau: a, tau0corr: b, extinction: c, extinctionCorrected: d, stdDevTau: e, tauError: f, tauCorrected: g, tau0: h, max: i, ringdowns: j};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var _user$project$Crd$decodeExtData = A3(
	_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
	'Ringdowns',
	_elm_lang$core$Json_Decode$list(
		_elm_lang$core$Json_Decode$list(_elm_lang$core$Json_Decode$int)),
	A3(
		_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$requiredAt,
		{
			ctor: '::',
			_0: 'extParam',
			_1: {
				ctor: '::',
				_0: 'max',
				_1: {ctor: '[]'}
			}
		},
		_elm_lang$core$Json_Decode$float,
		A3(
			_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$requiredAt,
			{
				ctor: '::',
				_0: 'extParam',
				_1: {
					ctor: '::',
					_0: 'Tau0',
					_1: {ctor: '[]'}
				}
			},
			_elm_lang$core$Json_Decode$float,
			A3(
				_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$requiredAt,
				{
					ctor: '::',
					_0: 'extParam',
					_1: {
						ctor: '::',
						_0: 'taucorr',
						_1: {ctor: '[]'}
					}
				},
				_elm_lang$core$Json_Decode$float,
				A3(
					_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$requiredAt,
					{
						ctor: '::',
						_0: 'extParam',
						_1: {
							ctor: '::',
							_0: 'eTau',
							_1: {ctor: '[]'}
						}
					},
					_elm_lang$core$Json_Decode$float,
					A3(
						_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$requiredAt,
						{
							ctor: '::',
							_0: 'extParam',
							_1: {
								ctor: '::',
								_0: 'stdevTau',
								_1: {ctor: '[]'}
							}
						},
						_elm_lang$core$Json_Decode$float,
						A3(
							_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$requiredAt,
							{
								ctor: '::',
								_0: 'extParam',
								_1: {
									ctor: '::',
									_0: 'extCorr',
									_1: {ctor: '[]'}
								}
							},
							_elm_lang$core$Json_Decode$float,
							A3(
								_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$requiredAt,
								{
									ctor: '::',
									_0: 'extParam',
									_1: {
										ctor: '::',
										_0: 'ext',
										_1: {ctor: '[]'}
									}
								},
								_elm_lang$core$Json_Decode$float,
								A3(
									_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$requiredAt,
									{
										ctor: '::',
										_0: 'extParam',
										_1: {
											ctor: '::',
											_0: 'Tau0cor',
											_1: {ctor: '[]'}
										}
									},
									_elm_lang$core$Json_Decode$float,
									A3(
										_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$requiredAt,
										{
											ctor: '::',
											_0: 'extParam',
											_1: {
												ctor: '::',
												_0: 'Tau',
												_1: {ctor: '[]'}
											}
										},
										_elm_lang$core$Json_Decode$float,
										_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$decode(_user$project$Crd$CrdsCell)))))))))));
var _user$project$Crd$retrieveCrdData = F3(
	function (head, data, cell_data) {
		return A2(
			_elm_lang$core$Result$withDefault,
			cell_data,
			A2(
				_elm_lang$core$Json_Decode$decodeString,
				A2(
					_elm_lang$core$Json_Decode$field,
					head,
					_elm_lang$core$Json_Decode$array(_user$project$Crd$decodeExtData)),
				data));
	});
var _user$project$Crd$viewRingdown = function (model) {
	var cell_0 = A2(
		_elm_lang$core$Maybe$withDefault,
		_user$project$Crd$CrdsCell(0)(0)(0)(0)(0)(0)(0)(0)(0)(
			{
				ctor: '::',
				_0: {
					ctor: '::',
					_0: 0,
					_1: {ctor: '[]'}
				},
				_1: {ctor: '[]'}
			}),
		A2(_elm_lang$core$Array$get, 0, model.data));
	var raw_data = A2(
		_elm_lang$core$Maybe$withDefault,
		{ctor: '[]'},
		_elm_lang$core$List$head(cell_0.ringdowns));
	var data = A2(
		_elm_lang$core$List$indexedMap,
		F2(
			function (v0, v1) {
				return {ctor: '_Tuple2', _0: v0, _1: v1};
			}),
		raw_data);
	var blueCircle = function (_p1) {
		var _p2 = _p1;
		return A3(
			_terezka$elm_plot$Plot$dot,
			A2(_terezka$elm_plot$Plot$viewCircle, 5, '#cfd8ea'),
			_p2._0,
			_p2._1 * 1.2);
	};
	var line = {
		axis: _terezka$elm_plot$Plot$normalAxis,
		interpolation: A2(
			_terezka$elm_plot$Plot$Linear,
			_elm_lang$core$Maybe$Nothing,
			{
				ctor: '::',
				_0: _elm_lang$svg$Svg_Attributes$stroke('#cfd8ea'),
				_1: {ctor: '[]'}
			}),
		toDataPoints: _elm_lang$core$List$map(blueCircle)
	};
	return A2(
		_terezka$elm_plot$Plot$viewSeries,
		{
			ctor: '::',
			_0: _terezka$elm_plot$Plot$line(
				_elm_lang$core$List$map(
					function (_p3) {
						var _p4 = _p3;
						return A2(
							_terezka$elm_plot$Plot$circle,
							_elm_lang$core$Basics$toFloat(_p4._0),
							_elm_lang$core$Basics$toFloat(_p4._1));
					})),
			_1: {ctor: '[]'}
		},
		data);
};
var _user$project$Crd$CrdCvt = F5(
	function (a, b, c, d, e) {
		return {labels: a, rate: b, dc: c, heater: d, power: e};
	});
var _user$project$Crd$Heater = F2(
	function (a, b) {
		return {pid: a, sp: b};
	});
var _user$project$Crd$defaultCvt = A5(
	_user$project$Crd$CrdCvt,
	{
		ctor: '::',
		_0: 'cell_0',
		_1: {
			ctor: '::',
			_0: 'cell_1',
			_1: {ctor: '[]'}
		}
	},
	1000,
	400,
	A2(
		_user$project$Crd$Heater,
		_elm_lang$core$Array$fromList(
			{
				ctor: '::',
				_0: '1',
				_1: {
					ctor: '::',
					_0: '0',
					_1: {
						ctor: '::',
						_0: '0',
						_1: {ctor: '[]'}
					}
				}
			}),
		'18'),
	false);
var _user$project$Crd$init = {
	cvt: _user$project$Crd$defaultCvt,
	data: _elm_lang$core$Array$fromList(
		{
			ctor: '::',
			_0: _user$project$Crd$CrdsCell(0)(0)(0)(0)(0)(0)(0)(0)(0)(
				{
					ctor: '::',
					_0: {
						ctor: '::',
						_0: 0,
						_1: {ctor: '[]'}
					},
					_1: {ctor: '[]'}
				}),
			_1: {ctor: '[]'}
		})
};
var _user$project$Crd$decodeHeater = A3(
	_elm_lang$core$Json_Decode$map2,
	_user$project$Crd$Heater,
	A2(
		_elm_lang$core$Json_Decode$field,
		'pid',
		_elm_lang$core$Json_Decode$array(_elm_lang$core$Json_Decode$string)),
	A2(_elm_lang$core$Json_Decode$field, 'sp', _elm_lang$core$Json_Decode$string));
var _user$project$Crd$decodeCvt = function (cvt) {
	return A4(
		_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$optional,
		'laser_enable',
		_elm_lang$core$Json_Decode$bool,
		cvt.power,
		A3(
			_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
			'heater',
			_user$project$Crd$decodeHeater,
			A3(
				_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
				'samp_per_cycle',
				_elm_lang$core$Json_Decode$int,
				A3(
					_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
					'f',
					_elm_lang$core$Json_Decode$int,
					A3(
						_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
						'cell_ids',
						_elm_lang$core$Json_Decode$list(_elm_lang$core$Json_Decode$string),
						_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$decode(_user$project$Crd$CrdCvt))))));
};
var _user$project$Crd$retrieveCrdCvt = F3(
	function (heading, data, cvt) {
		return _elm_lang$core$Native_Utils.eq(data, '{}') ? cvt : A2(
			_elm_lang$core$Result$withDefault,
			cvt,
			A2(
				_elm_lang$core$Json_Decode$decodeString,
				A2(
					_elm_lang$core$Json_Decode$field,
					heading,
					_user$project$Crd$decodeCvt(cvt)),
				data));
	});
var _user$project$Crd$TogglePower = {ctor: 'TogglePower'};
var _user$project$Crd$UpdateDutyCycle = function (a) {
	return {ctor: 'UpdateDutyCycle', _0: a};
};
var _user$project$Crd$UpdateFrequency = function (a) {
	return {ctor: 'UpdateFrequency', _0: a};
};

var _user$project$Devices_Device$checkEmptyDevice = function (devt) {
	return !_elm_lang$core$Native_Utils.eq(
		_elm_lang$core$Tuple$first(devt),
		'');
};
var _user$project$Devices_Device$defaultDevice = {type_: 'n', label: 'label', sn: '-1', controller: false, address: '-1', model: 'not_present', active: false, sp: _elm_lang$core$Maybe$Nothing};
var _user$project$Devices_Device$setSpIn = F2(
	function (sp, dev) {
		return _elm_lang$core$Native_Utils.update(
			dev,
			{
				sp: _elm_lang$core$Maybe$Just(sp)
			});
	});
var _user$project$Devices_Device$Device = F8(
	function (a, b, c, d, e, f, g, h) {
		return {type_: a, label: b, sn: c, controller: d, address: e, model: f, active: g, sp: h};
	});
var _user$project$Devices_Device$decodeDevice = A9(
	_elm_lang$core$Json_Decode$map8,
	_user$project$Devices_Device$Device,
	A2(_elm_lang$core$Json_Decode$field, 'type', _elm_lang$core$Json_Decode$string),
	A2(_elm_lang$core$Json_Decode$field, 'label', _elm_lang$core$Json_Decode$string),
	A2(_elm_lang$core$Json_Decode$field, 'sn', _elm_lang$core$Json_Decode$string),
	A2(_elm_lang$core$Json_Decode$field, 'controller', _elm_lang$core$Json_Decode$bool),
	A2(_elm_lang$core$Json_Decode$field, 'address', _elm_lang$core$Json_Decode$string),
	A2(_elm_lang$core$Json_Decode$field, 'model', _elm_lang$core$Json_Decode$string),
	A2(_elm_lang$core$Json_Decode$field, 'active', _elm_lang$core$Json_Decode$bool),
	_elm_lang$core$Json_Decode$maybe(
		A2(_elm_lang$core$Json_Decode$field, 'sp', _elm_lang$core$Json_Decode$string)));
var _user$project$Devices_Device$decodeDeviceCvt = A2(
	_elm_lang$core$Json_Decode$field,
	'device',
	_elm_lang$core$Json_Decode$dict(_user$project$Devices_Device$decodeDevice));
var _user$project$Devices_Device$defaultDeviceDict = A2(
	_elm_lang$core$Dict$singleton,
	'null_device',
	A8(_user$project$Devices_Device$Device, 'n', 'label', '-1', false, '-1', 'not_present', false, _elm_lang$core$Maybe$Nothing));
var _user$project$Devices_Device$Vaisala = {ctor: 'Vaisala'};
var _user$project$Devices_Device$Alicat = {ctor: 'Alicat'};
var _user$project$Devices_Device$Ppt = {ctor: 'Ppt'};

var _user$project$Devices_Alicat$update = F2(
	function (msg, model) {
		var _p0 = msg;
		if (_p0.ctor === 'UpdateSetpoint') {
			var _p2 = _p0._0;
			var _p1 = A2(_elm_lang$core$Dict$get, _p2, model.cvt);
			if (_p1.ctor === 'Just') {
				var n_device = A2(_user$project$Devices_Device$setSpIn, _p0._1, _p1._0);
				var n_dict = A3(_elm_lang$core$Dict$insert, _p2, n_device, model.cvt);
				return _elm_lang$core$Native_Utils.update(
					model,
					{cvt: n_dict});
			} else {
				return model;
			}
		} else {
			return model;
		}
	});
var _user$project$Devices_Alicat$insertAlicatDev = F2(
	function (devs, model) {
		var adev = A2(
			_elm_lang$core$List$map,
			function (dev) {
				var device = _elm_lang$core$Tuple$second(dev);
				var d = _elm_lang$core$Native_Utils.eq(device.type_, 'alicat') ? dev : {ctor: '_Tuple2', _0: '', _1: _user$project$Devices_Device$defaultDevice};
				return d;
			},
			_elm_lang$core$Dict$toList(devs));
		var a_dev = A2(_elm_lang$core$List$filter, _user$project$Devices_Device$checkEmptyDevice, adev);
		return _elm_lang$core$Native_Utils.update(
			model,
			{
				cvt: _elm_lang$core$Dict$fromList(a_dev)
			});
	});
var _user$project$Devices_Alicat$asCvtIn = F2(
	function (model, dict) {
		return _elm_lang$core$Native_Utils.update(
			model,
			{cvt: dict});
	});
var _user$project$Devices_Alicat$Data = F5(
	function (a, b, c, d, e) {
		return {pressure: a, temperature: b, setpoint: c, mass_flow: d, output: e};
	});
var _user$project$Devices_Alicat$defaultDictData = A2(
	_elm_lang$core$Dict$singleton,
	'Default_Alicat',
	A5(_user$project$Devices_Alicat$Data, _elm_lang$core$Maybe$Nothing, _elm_lang$core$Maybe$Nothing, _elm_lang$core$Maybe$Nothing, _elm_lang$core$Maybe$Nothing, 0));
var _user$project$Devices_Alicat$init = {data: _user$project$Devices_Alicat$defaultDictData, cvt: _user$project$Devices_Device$defaultDeviceDict};
var _user$project$Devices_Alicat$defaultData = A5(_user$project$Devices_Alicat$Data, _elm_lang$core$Maybe$Nothing, _elm_lang$core$Maybe$Nothing, _elm_lang$core$Maybe$Nothing, _elm_lang$core$Maybe$Nothing, 0);
var _user$project$Devices_Alicat$decodeData = A6(
	_elm_lang$core$Json_Decode$map5,
	_user$project$Devices_Alicat$Data,
	_elm_lang$core$Json_Decode$maybe(
		A2(_elm_lang$core$Json_Decode$field, 'P', _elm_lang$core$Json_Decode$float)),
	_elm_lang$core$Json_Decode$maybe(
		A2(_elm_lang$core$Json_Decode$field, 'T', _elm_lang$core$Json_Decode$float)),
	_elm_lang$core$Json_Decode$maybe(
		A2(_elm_lang$core$Json_Decode$field, 'Qsp', _elm_lang$core$Json_Decode$float)),
	_elm_lang$core$Json_Decode$maybe(
		A2(_elm_lang$core$Json_Decode$field, 'Q0', _elm_lang$core$Json_Decode$float)),
	A2(_elm_lang$core$Json_Decode$field, 'Q', _elm_lang$core$Json_Decode$float));
var _user$project$Devices_Alicat$getAlicatData = F2(
	function (model, data) {
		var test = A2(
			_elm_lang$core$List$map,
			function (d) {
				var id = _elm_lang$core$Tuple$first(d);
				var d_ = A2(
					_elm_lang$core$Result$withDefault,
					_user$project$Devices_Alicat$defaultData,
					A2(
						_elm_lang$core$Json_Decode$decodeString,
						A2(_elm_lang$core$Json_Decode$field, id, _user$project$Devices_Alicat$decodeData),
						data));
				return {ctor: '_Tuple2', _0: id, _1: d_};
			},
			_elm_lang$core$Dict$toList(model.cvt));
		return _elm_lang$core$Native_Utils.update(
			model,
			{
				data: _elm_lang$core$Dict$fromList(test)
			});
	});
var _user$project$Devices_Alicat$Model = F2(
	function (a, b) {
		return {cvt: a, data: b};
	});
var _user$project$Devices_Alicat$P = {ctor: 'P'};
var _user$project$Devices_Alicat$PC = {ctor: 'PC'};
var _user$project$Devices_Alicat$V = {ctor: 'V'};
var _user$project$Devices_Alicat$VC = {ctor: 'VC'};
var _user$project$Devices_Alicat$M = {ctor: 'M'};
var _user$project$Devices_Alicat$MC = {ctor: 'MC'};
var _user$project$Devices_Alicat$dtype = function () {
	var helper = function (s) {
		return _elm_lang$core$Native_Utils.eq(s, 'MC') ? _elm_lang$core$Json_Decode$succeed(_user$project$Devices_Alicat$MC) : (_elm_lang$core$Native_Utils.eq(s, 'VC') ? _elm_lang$core$Json_Decode$succeed(_user$project$Devices_Alicat$VC) : (_elm_lang$core$Native_Utils.eq(s, 'V') ? _elm_lang$core$Json_Decode$succeed(_user$project$Devices_Alicat$V) : (_elm_lang$core$Native_Utils.eq(s, 'M') ? _elm_lang$core$Json_Decode$succeed(_user$project$Devices_Alicat$M) : (_elm_lang$core$Native_Utils.eq(s, 'P') ? _elm_lang$core$Json_Decode$succeed(_user$project$Devices_Alicat$P) : (_elm_lang$core$Native_Utils.eq(s, 'PC') ? _elm_lang$core$Json_Decode$succeed(_user$project$Devices_Alicat$PC) : _elm_lang$core$Json_Decode$fail('Failed to decode dtype'))))));
	};
	return A2(_elm_lang$core$Json_Decode$andThen, helper, _elm_lang$core$Json_Decode$string);
}();
var _user$project$Devices_Alicat$SendNewSetpoint = function (a) {
	return {ctor: 'SendNewSetpoint', _0: a};
};
var _user$project$Devices_Alicat$UpdateSetpoint = F2(
	function (a, b) {
		return {ctor: 'UpdateSetpoint', _0: a, _1: b};
	});

var _user$project$Devices_Ppt$insertPptDev = F2(
	function (devs, model) {
		var pdev = A2(
			_elm_lang$core$List$map,
			function (dev) {
				var device = _elm_lang$core$Tuple$second(dev);
				var d = _elm_lang$core$Native_Utils.eq(device.type_, 'ppt') ? dev : {ctor: '_Tuple2', _0: '', _1: _user$project$Devices_Device$defaultDevice};
				return d;
			},
			_elm_lang$core$Dict$toList(devs));
		var dev_ = A2(_elm_lang$core$List$filter, _user$project$Devices_Device$checkEmptyDevice, pdev);
		return _elm_lang$core$Native_Utils.update(
			model,
			{
				cvt: _elm_lang$core$Dict$fromList(dev_)
			});
	});
var _user$project$Devices_Ppt$defaultCvt = A2(_elm_lang$core$Dict$singleton, 'NullDevice', _user$project$Devices_Device$defaultDevice);
var _user$project$Devices_Ppt$Model = F2(
	function (a, b) {
		return {cvt: a, data: b};
	});
var _user$project$Devices_Ppt$Data = F2(
	function (a, b) {
		return {pressure: a, temperature: b};
	});
var _user$project$Devices_Ppt$defaultData = A2(
	_elm_lang$core$Dict$singleton,
	'NullData',
	A2(_user$project$Devices_Ppt$Data, 0, 0));
var _user$project$Devices_Ppt$init = A2(_user$project$Devices_Ppt$Model, _user$project$Devices_Ppt$defaultCvt, _user$project$Devices_Ppt$defaultData);
var _user$project$Devices_Ppt$decodeData = A3(
	_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
	't',
	_elm_lang$core$Json_Decode$float,
	A3(
		_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
		'p',
		_elm_lang$core$Json_Decode$float,
		_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$decode(_user$project$Devices_Ppt$Data)));
var _user$project$Devices_Ppt$getPptData = F2(
	function (model, data) {
		var d = A2(
			_elm_lang$core$List$map,
			function (d) {
				var id = _elm_lang$core$Tuple$first(d);
				var d_ = A2(
					_elm_lang$core$Result$withDefault,
					A2(_user$project$Devices_Ppt$Data, 0, 0),
					A2(
						_elm_lang$core$Json_Decode$decodeString,
						A2(_elm_lang$core$Json_Decode$field, id, _user$project$Devices_Ppt$decodeData),
						data));
				return {ctor: '_Tuple2', _0: id, _1: d_};
			},
			_elm_lang$core$Dict$toList(model.cvt));
		return _elm_lang$core$Native_Utils.update(
			model,
			{
				data: _elm_lang$core$Dict$fromList(d)
			});
	});

var _user$project$Devices_Vaisala$insertVaisalaDev = F2(
	function (devs, model) {
		var vdev = A2(
			_elm_lang$core$List$map,
			function (dev) {
				var device = _elm_lang$core$Tuple$second(dev);
				var d = _elm_lang$core$Native_Utils.eq(device.type_, 'vaisala') ? dev : {ctor: '_Tuple2', _0: '', _1: _user$project$Devices_Device$defaultDevice};
				return d;
			},
			_elm_lang$core$Dict$toList(devs));
		var v_dev = A2(_elm_lang$core$List$filter, _user$project$Devices_Device$checkEmptyDevice, vdev);
		return _elm_lang$core$Native_Utils.update(
			model,
			{
				cvt: _elm_lang$core$Dict$fromList(v_dev)
			});
	});
var _user$project$Devices_Vaisala$defaultCvt = A2(_elm_lang$core$Dict$singleton, 'NullDevice', _user$project$Devices_Device$defaultDevice);
var _user$project$Devices_Vaisala$Model = F2(
	function (a, b) {
		return {cvt: a, data: b};
	});
var _user$project$Devices_Vaisala$Data = F3(
	function (a, b, c) {
		return {temperature: a, relative_humidity: b, dewpoint: c};
	});
var _user$project$Devices_Vaisala$defaultData = A2(
	_elm_lang$core$Dict$singleton,
	'NullData',
	A3(_user$project$Devices_Vaisala$Data, 0, 0, 0));
var _user$project$Devices_Vaisala$init = A2(_user$project$Devices_Vaisala$Model, _user$project$Devices_Vaisala$defaultCvt, _user$project$Devices_Vaisala$defaultData);
var _user$project$Devices_Vaisala$decodeData = A3(
	_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
	'td',
	_elm_lang$core$Json_Decode$float,
	A3(
		_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
		'rh',
		_elm_lang$core$Json_Decode$float,
		A3(
			_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
			't',
			_elm_lang$core$Json_Decode$float,
			_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$decode(_user$project$Devices_Vaisala$Data))));
var _user$project$Devices_Vaisala$getVaisalaData = F2(
	function (model, data) {
		var test = A2(
			_elm_lang$core$List$map,
			function (d) {
				var id = _elm_lang$core$Tuple$first(d);
				var d_ = A2(
					_elm_lang$core$Result$withDefault,
					A3(_user$project$Devices_Vaisala$Data, 0, 0, 0),
					A2(
						_elm_lang$core$Json_Decode$decodeString,
						A2(_elm_lang$core$Json_Decode$field, id, _user$project$Devices_Vaisala$decodeData),
						data));
				return {ctor: '_Tuple2', _0: id, _1: d_};
			},
			_elm_lang$core$Dict$toList(model.cvt));
		return _elm_lang$core$Native_Utils.update(
			model,
			{
				data: _elm_lang$core$Dict$fromList(test)
			});
	});

var _user$project$GcrdTypes$setCalibration = F2(
	function (cal, cvt) {
		return _elm_lang$core$Native_Utils.update(
			cvt,
			{cal_state: cal});
	});
var _user$project$GcrdTypes$switchPump = function (cvt) {
	return _elm_lang$core$Native_Utils.update(
		cvt,
		{pump: !cvt.pump});
};
var _user$project$GcrdTypes$setFanVoltage = F2(
	function (volts, cvt) {
		return _elm_lang$core$Native_Utils.update(
			cvt,
			{fan_voltage: volts});
	});
var _user$project$GcrdTypes$setFanEnable = F2(
	function (val, cvt) {
		return _elm_lang$core$Native_Utils.update(
			cvt,
			{fan: val});
	});
var _user$project$GcrdTypes$setCalibrationCvt = F2(
	function (cal, cvt) {
		return _elm_lang$core$Native_Utils.update(
			cvt,
			{cal_state: cal});
	});
var _user$project$GcrdTypes$asCalibrationIn = _elm_lang$core$Basics$flip(_user$project$GcrdTypes$setCalibrationCvt);
var _user$project$GcrdTypes$toggleUVLampPosition = function (cal) {
	return _elm_lang$core$Native_Utils.update(
		cal,
		{uv_lamp: !cal.uv_lamp});
};
var _user$project$GcrdTypes$toggleO2AddPosition = function (cal) {
	return _elm_lang$core$Native_Utils.update(
		cal,
		{o2_add: !cal.o2_add});
};
var _user$project$GcrdTypes$toggleO3AddPosition = function (cal) {
	return _elm_lang$core$Native_Utils.update(
		cal,
		{o3_add: !cal.o3_add});
};
var _user$project$GcrdTypes$temperatureToArray = function (t) {
	return _elm_lang$core$Array$fromList(
		{
			ctor: '::',
			_0: t.pasCell1,
			_1: {
				ctor: '::',
				_0: t.pasCell2,
				_1: {
					ctor: '::',
					_0: t.pasLaserHead1,
					_1: {
						ctor: '::',
						_0: t.pasLaserHead2,
						_1: {
							ctor: '::',
							_0: t.cjc0,
							_1: {
								ctor: '::',
								_0: t.boxExit,
								_1: {
									ctor: '::',
									_0: t.crdHeater,
									_1: {
										ctor: '::',
										_0: t.boxInlet,
										_1: {
											ctor: '::',
											_0: t.crdLaserHead,
											_1: {
												ctor: '::',
												_0: t.cjc1,
												_1: {ctor: '[]'}
											}
										}
									}
								}
							}
						}
					}
				}
			}
		});
};
var _user$project$GcrdTypes$getTemperature = F2(
	function (index, t) {
		return A2(
			_elm_lang$core$Maybe$withDefault,
			0,
			A2(
				_elm_lang$core$Array$get,
				index,
				_user$project$GcrdTypes$temperatureToArray(t)));
	});
var _user$project$GcrdTypes$setFilter = F2(
	function (filt, cvt) {
		return _elm_lang$core$Native_Utils.update(
			cvt,
			{filter: filt});
	});
var _user$project$GcrdTypes$asFilterIn = _elm_lang$core$Basics$flip(_user$project$GcrdTypes$setFilter);
var _user$project$GcrdTypes$toggleFilterPosition = function (f) {
	var val = _elm_lang$core$Native_Utils.eq(f.pos, 0) ? 1 : 0;
	return _elm_lang$core$Native_Utils.update(
		f,
		{pos: val});
};
var _user$project$GcrdTypes$setMessages = F2(
	function (msgs, data) {
		return _elm_lang$core$Native_Utils.update(
			data,
			{msg: msgs});
	});
var _user$project$GcrdTypes$defaultPptData = {id: 'default', pressure: 0, temperature: 0};
var _user$project$GcrdTypes$defaultVaisalaData = {id: 'default', temperature: 0, relative_humidity: 0, dewpoint: 0};
var _user$project$GcrdTypes$VaisalaData = F4(
	function (a, b, c, d) {
		return {id: a, temperature: b, relative_humidity: c, dewpoint: d};
	});
var _user$project$GcrdTypes$PptData = F3(
	function (a, b, c) {
		return {id: a, pressure: b, temperature: c};
	});
var _user$project$GcrdTypes$Data = F4(
	function (a, b, c, d) {
		return {time: a, msg: b, interlock: c, temperatures: d};
	});
var _user$project$GcrdTypes$Filter = function (a) {
	return {pos: a};
};
var _user$project$GcrdTypes$decodeFilter = A2(
	_elm_lang$core$Json_Decode$map,
	_user$project$GcrdTypes$Filter,
	A2(_elm_lang$core$Json_Decode$field, 'cell', _elm_lang$core$Json_Decode$int));
var _user$project$GcrdTypes$Temperatures = function (a) {
	return function (b) {
		return function (c) {
			return function (d) {
				return function (e) {
					return function (f) {
						return function (g) {
							return function (h) {
								return function (i) {
									return function (j) {
										return {pasCell1: a, pasCell2: b, pasLaserHead1: c, pasLaserHead2: d, cjc0: e, boxExit: f, crdHeater: g, boxInlet: h, crdLaserHead: i, cjc1: j};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var _user$project$GcrdTypes$defaultData = A4(
	_user$project$GcrdTypes$Data,
	'0',
	{ctor: '[]'},
	false,
	_user$project$GcrdTypes$Temperatures(0)(0)(0)(0)(0)(0)(0)(0)(0)(0));
var _user$project$GcrdTypes$decodeTemperatures = A3(
	_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
	'cjc1',
	_elm_lang$core$Json_Decode$float,
	A3(
		_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
		'crd_laser_head',
		_elm_lang$core$Json_Decode$float,
		A3(
			_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
			'box_inlet',
			_elm_lang$core$Json_Decode$float,
			A3(
				_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
				'crd_heater',
				_elm_lang$core$Json_Decode$float,
				A3(
					_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
					'box_exit',
					_elm_lang$core$Json_Decode$float,
					A3(
						_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
						'cjc0',
						_elm_lang$core$Json_Decode$float,
						A3(
							_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
							'pas_las_head_2',
							_elm_lang$core$Json_Decode$float,
							A3(
								_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
								'pas_las_head_1',
								_elm_lang$core$Json_Decode$float,
								A3(
									_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
									'pas_cell_2',
									_elm_lang$core$Json_Decode$float,
									A3(
										_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
										'pas_cell_1',
										_elm_lang$core$Json_Decode$float,
										_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$decode(_user$project$GcrdTypes$Temperatures)))))))))));
var _user$project$GcrdTypes$decodeData = A3(
	_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
	'temperatures',
	_user$project$GcrdTypes$decodeTemperatures,
	A3(
		_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
		'interlock',
		_elm_lang$core$Json_Decode$bool,
		A3(
			_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
			'msgs',
			_elm_lang$core$Json_Decode$list(_elm_lang$core$Json_Decode$string),
			A3(
				_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
				'Time',
				_elm_lang$core$Json_Decode$string,
				_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$decode(_user$project$GcrdTypes$Data)))));
var _user$project$GcrdTypes$Calibration = F3(
	function (a, b, c) {
		return {o2_add: a, o3_add: b, uv_lamp: c};
	});
var _user$project$GcrdTypes$decodeCal = A4(
	_elm_lang$core$Json_Decode$map3,
	_user$project$GcrdTypes$Calibration,
	A2(_elm_lang$core$Json_Decode$field, 'o2_add', _elm_lang$core$Json_Decode$bool),
	A2(_elm_lang$core$Json_Decode$field, 'o3_add', _elm_lang$core$Json_Decode$bool),
	A2(_elm_lang$core$Json_Decode$field, 'uv_lamp', _elm_lang$core$Json_Decode$bool));
var _user$project$GcrdTypes$defaultCvtData = {
	pump: false,
	cal_state: A3(_user$project$GcrdTypes$Calibration, false, false, false),
	devIds: {ctor: '[]'},
	filter: _user$project$GcrdTypes$Filter(0),
	fan: false,
	fan_voltage: 0,
	sequence_state: 'Pause',
	save: false
};
var _user$project$GcrdTypes$Cvt = F8(
	function (a, b, c, d, e, f, g, h) {
		return {pump: a, cal_state: b, devIds: c, filter: d, fan: e, fan_voltage: f, sequence_state: g, save: h};
	});
var _user$project$GcrdTypes$decodeCvt = function (cvt) {
	return A3(
		_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$requiredAt,
		{
			ctor: '::',
			_0: 'general',
			_1: {
				ctor: '::',
				_0: 'save',
				_1: {ctor: '[]'}
			}
		},
		_elm_lang$core$Json_Decode$bool,
		A3(
			_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$requiredAt,
			{
				ctor: '::',
				_0: 'general',
				_1: {
					ctor: '::',
					_0: 'seq_state',
					_1: {ctor: '[]'}
				}
			},
			_elm_lang$core$Json_Decode$string,
			A3(
				_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$requiredAt,
				{
					ctor: '::',
					_0: 'general',
					_1: {
						ctor: '::',
						_0: 'fan_speed',
						_1: {ctor: '[]'}
					}
				},
				_elm_lang$core$Json_Decode$float,
				A3(
					_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$requiredAt,
					{
						ctor: '::',
						_0: 'general',
						_1: {
							ctor: '::',
							_0: 'fan',
							_1: {ctor: '[]'}
						}
					},
					_elm_lang$core$Json_Decode$bool,
					A3(
						_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
						'filter',
						_user$project$GcrdTypes$decodeFilter,
						A3(
							_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$requiredAt,
							{
								ctor: '::',
								_0: 'general',
								_1: {
									ctor: '::',
									_0: 'dev_ids',
									_1: {ctor: '[]'}
								}
							},
							_elm_lang$core$Json_Decode$list(_elm_lang$core$Json_Decode$string),
							A3(
								_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
								'calibration',
								_user$project$GcrdTypes$decodeCal,
								A3(
									_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$requiredAt,
									{
										ctor: '::',
										_0: 'general',
										_1: {
											ctor: '::',
											_0: 'vacuum_pump',
											_1: {ctor: '[]'}
										}
									},
									_elm_lang$core$Json_Decode$bool,
									_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$decode(_user$project$GcrdTypes$Cvt)))))))));
};

var _user$project$Network$buildAddress = function (model) {
	return A2(
		_elm_lang$core$Basics_ops['++'],
		'http://',
		A2(
			_elm_lang$core$Basics_ops['++'],
			model.ip,
			A2(
				_elm_lang$core$Basics_ops['++'],
				':',
				A2(
					_elm_lang$core$Basics_ops['++'],
					model.port_,
					A2(
						_elm_lang$core$Basics_ops['++'],
						'/',
						A2(_elm_lang$core$Basics_ops['++'], model.service, '/'))))));
};
var _user$project$Network$init = function (model) {
	return model;
};
var _user$project$Network$update = F2(
	function (msg, model) {
		var _p0 = msg;
		if (_p0.ctor === 'UpdateIP') {
			return _elm_lang$core$Native_Utils.update(
				model,
				{ip: _p0._0});
		} else {
			return _elm_lang$core$Native_Utils.update(
				model,
				{port_: _p0._0});
		}
	});
var _user$project$Network$updateIpConfig = _elm_lang$core$Native_Platform.outgoingPort(
	'updateIpConfig',
	function (v) {
		return _elm_lang$core$Native_List.toArray(v).map(
			function (v) {
				return v;
			});
	});
var _user$project$Network$initPort = _elm_lang$core$Native_Platform.incomingPort(
	'initPort',
	A2(
		_elm_lang$core$Json_Decode$andThen,
		function (x0) {
			return A2(
				_elm_lang$core$Json_Decode$andThen,
				function (x1) {
					return _elm_lang$core$Json_Decode$succeed(
						{ctor: '_Tuple2', _0: x0, _1: x1});
				},
				A2(_elm_lang$core$Json_Decode$index, 1, _elm_lang$core$Json_Decode$string));
		},
		A2(_elm_lang$core$Json_Decode$index, 0, _elm_lang$core$Json_Decode$string)));
var _user$project$Network$Model = F3(
	function (a, b, c) {
		return {ip: a, port_: b, service: c};
	});
var _user$project$Network$UpdatePort = function (a) {
	return {ctor: 'UpdatePort', _0: a};
};
var _user$project$Network$UpdateIP = function (a) {
	return {ctor: 'UpdateIP', _0: a};
};

var _user$project$Pas$floatString = A2(_elm_lang$core$Json_Decode$map, _elm_lang$core$Basics$toString, _elm_lang$core$Json_Decode$float);
var _user$project$Pas$intString = A2(_elm_lang$core$Json_Decode$map, _elm_lang$core$Basics$toString, _elm_lang$core$Json_Decode$int);
var _user$project$Pas$setCellData = F3(
	function (model, cint, cell) {
		var n_data = model.data;
		var newCell = A3(_elm_lang$core$Array$set, cint, cell, model.data.cell);
		var nn_data = _elm_lang$core$Native_Utils.update(
			n_data,
			{cell: newCell});
		return _elm_lang$core$Native_Utils.update(
			model,
			{data: nn_data});
	});
var _user$project$Pas$asFreqIn = F2(
	function (cell, freqData) {
		return _elm_lang$core$Native_Utils.update(
			cell,
			{frequencyData: freqData});
	});
var _user$project$Pas$toggleLaserPower = F2(
	function (cell, cvt) {
		var new_cvt = function () {
			var _p0 = cell;
			switch (_p0) {
				case 0:
					return _elm_lang$core$Native_Utils.update(
						cvt,
						{enable_0: !cvt.enable_0});
				case 1:
					return _elm_lang$core$Native_Utils.update(
						cvt,
						{enable_1: !cvt.enable_1});
				default:
					return cvt;
			}
		}();
		return new_cvt;
	});
var _user$project$Pas$setFrequency1 = F2(
	function (f, cvt) {
		return _elm_lang$core$Native_Utils.update(
			cvt,
			{fmod_1: f});
	});
var _user$project$Pas$setFrequency0 = F2(
	function (f, cvt) {
		return _elm_lang$core$Native_Utils.update(
			cvt,
			{fmod_0: f});
	});
var _user$project$Pas$setSpk = F2(
	function (spk, cvt) {
		return _elm_lang$core$Native_Utils.update(
			cvt,
			{spk: spk});
	});
var _user$project$Pas$asSpeakerIn = _elm_lang$core$Basics$flip(_user$project$Pas$setSpk);
var _user$project$Pas$asCvtIn = F2(
	function (model, cvt) {
		return _elm_lang$core$Native_Utils.update(
			model,
			{cvt: cvt});
	});
var _user$project$Pas$setPasHeater1 = F2(
	function (htr, cvt) {
		return _elm_lang$core$Native_Utils.update(
			cvt,
			{heater_1: htr});
	});
var _user$project$Pas$setPasHeater0 = F2(
	function (htr, cvt) {
		return _elm_lang$core$Native_Utils.update(
			cvt,
			{heater_0: htr});
	});
var _user$project$Pas$asHeater0In = _elm_lang$core$Basics$flip(_user$project$Pas$setPasHeater0);
var _user$project$Pas$asHeater1In = _elm_lang$core$Basics$flip(_user$project$Pas$setPasHeater0);
var _user$project$Pas$setSpkVscale = F2(
	function (vscale, spk) {
		return _elm_lang$core$Native_Utils.update(
			spk,
			{vscale: vscale});
	});
var _user$project$Pas$setHeaterPID = F2(
	function (pid, htr) {
		return _elm_lang$core$Native_Utils.update(
			htr,
			{pid: pid});
	});
var _user$project$Pas$setHeaterSP = F2(
	function (sp, htr) {
		return _elm_lang$core$Native_Utils.update(
			htr,
			{sp: sp});
	});
var _user$project$Pas$toggleSpeaker1Position = function (cvt) {
	return _elm_lang$core$Native_Utils.update(
		cvt,
		{speaker_1: !cvt.speaker_1});
};
var _user$project$Pas$toggleSpeaker0Position = function (cvt) {
	return _elm_lang$core$Native_Utils.update(
		cvt,
		{speaker_0: !cvt.speaker_0});
};
var _user$project$Pas$setDfSpk = F2(
	function (df, spk) {
		return _elm_lang$core$Native_Utils.update(
			spk,
			{df: df});
	});
var _user$project$Pas$setFcenterSpk = F2(
	function (fcenter, spk) {
		return _elm_lang$core$Native_Utils.update(
			spk,
			{center: fcenter});
	});
var _user$project$Pas$setVoffsetSpk = F2(
	function (voffset, spk) {
		return _elm_lang$core$Native_Utils.update(
			spk,
			{voffset: voffset});
	});
var _user$project$Pas$setVscaleSpk = F2(
	function (vscale, spk) {
		return _elm_lang$core$Native_Utils.update(
			spk,
			{vscale: vscale});
	});
var _user$project$Pas$asDataIn = F2(
	function (model, data) {
		return _elm_lang$core$Native_Utils.update(
			model,
			{data: data});
	});
var _user$project$Pas$update = F2(
	function (msg, model) {
		var _p1 = msg;
		switch (_p1.ctor) {
			case 'UpdateFreq':
				return model;
			case 'HandleGeneric':
				return model;
			case 'UpdateSpkVscale':
				var new_model = A2(
					_user$project$Pas$asCvtIn,
					model,
					A2(
						_user$project$Pas$asSpeakerIn,
						model.cvt,
						A2(_user$project$Pas$setSpkVscale, _p1._0, model.cvt.spk)));
				return new_model;
			case 'UpdateSpkVoffset':
				var new_model = A2(
					_user$project$Pas$asCvtIn,
					model,
					A2(
						_user$project$Pas$asSpeakerIn,
						model.cvt,
						A2(_user$project$Pas$setVoffsetSpk, _p1._0, model.cvt.spk)));
				return new_model;
			case 'UpdateSpkDf':
				var new_model = A2(
					_user$project$Pas$asCvtIn,
					model,
					A2(
						_user$project$Pas$asSpeakerIn,
						model.cvt,
						A2(_user$project$Pas$setDfSpk, _p1._0, model.cvt.spk)));
				return new_model;
			default:
				var new_model = A2(
					_user$project$Pas$asCvtIn,
					model,
					A2(
						_user$project$Pas$asSpeakerIn,
						model.cvt,
						A2(_user$project$Pas$setFcenterSpk, _p1._0, model.cvt.spk)));
				return new_model;
		}
	});
var _user$project$Pas$Model = F2(
	function (a, b) {
		return {cvt: a, data: b};
	});
var _user$project$Pas$PasData = F2(
	function (a, b) {
		return {cell: a, drive: b};
	});
var _user$project$Pas$PasCell = function (a) {
	return function (b) {
		return function (c) {
			return function (d) {
				return function (e) {
					return function (f) {
						return function (g) {
							return function (h) {
								return function (i) {
									return function (j) {
										return function (k) {
											return {resonant_frequency: a, integrated_area: b, q: c, max: d, absorption: e, laserRMS: f, laserDiodeData: g, frequencyData: h, timeData: i, fittedData: j, iaBackground: k};
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var _user$project$Pas$decodeCellData = A3(
	_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
	'IABack',
	_elm_lang$core$Json_Decode$float,
	A3(
		_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$requiredAt,
		{
			ctor: '::',
			_0: 'FittedData',
			_1: {
				ctor: '::',
				_0: 'Y',
				_1: {ctor: '[]'}
			}
		},
		_elm_lang$core$Json_Decode$list(_elm_lang$core$Json_Decode$float),
		A3(
			_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$requiredAt,
			{
				ctor: '::',
				_0: 'MicTime',
				_1: {
					ctor: '::',
					_0: 'Y',
					_1: {ctor: '[]'}
				}
			},
			_elm_lang$core$Json_Decode$list(_elm_lang$core$Json_Decode$int),
			A3(
				_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$requiredAt,
				{
					ctor: '::',
					_0: 'MicFreq',
					_1: {
						ctor: '::',
						_0: 'Y',
						_1: {ctor: '[]'}
					}
				},
				_elm_lang$core$Json_Decode$list(_elm_lang$core$Json_Decode$float),
				A3(
					_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$requiredAt,
					{
						ctor: '::',
						_0: 'PhotoDiode',
						_1: {
							ctor: '::',
							_0: 'Y',
							_1: {ctor: '[]'}
						}
					},
					_elm_lang$core$Json_Decode$list(_elm_lang$core$Json_Decode$int),
					A3(
						_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
						'lRMS',
						_elm_lang$core$Json_Decode$float,
						A3(
							_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$requiredAt,
							{
								ctor: '::',
								_0: 'derived',
								_1: {
									ctor: '::',
									_0: 'ext',
									_1: {ctor: '[]'}
								}
							},
							_elm_lang$core$Json_Decode$float,
							A3(
								_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$requiredAt,
								{
									ctor: '::',
									_0: 'derived',
									_1: {
										ctor: '::',
										_0: 'max',
										_1: {ctor: '[]'}
									}
								},
								_elm_lang$core$Json_Decode$list(_elm_lang$core$Json_Decode$float),
								A3(
									_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$requiredAt,
									{
										ctor: '::',
										_0: 'derived',
										_1: {
											ctor: '::',
											_0: 'Q',
											_1: {ctor: '[]'}
										}
									},
									_elm_lang$core$Json_Decode$float,
									A3(
										_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$requiredAt,
										{
											ctor: '::',
											_0: 'derived',
											_1: {
												ctor: '::',
												_0: 'IA',
												_1: {ctor: '[]'}
											}
										},
										_elm_lang$core$Json_Decode$float,
										A3(
											_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$requiredAt,
											{
												ctor: '::',
												_0: 'derived',
												_1: {
													ctor: '::',
													_0: 'f0',
													_1: {ctor: '[]'}
												}
											},
											_elm_lang$core$Json_Decode$float,
											_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$decode(_user$project$Pas$PasCell))))))))))));
var _user$project$Pas$truncateFrequencyData = F4(
	function (cell, min, max, model) {
		var defaultCell = _user$project$Pas$PasCell(0)(0)(0)(
			{
				ctor: '::',
				_0: 0,
				_1: {
					ctor: '::',
					_0: 0,
					_1: {ctor: '[]'}
				}
			})(0)(0)(
			{
				ctor: '::',
				_0: 0,
				_1: {ctor: '[]'}
			})(
			{
				ctor: '::',
				_0: 0,
				_1: {ctor: '[]'}
			})(
			{
				ctor: '::',
				_0: 0,
				_1: {ctor: '[]'}
			})(
			{
				ctor: '::',
				_0: 0,
				_1: {ctor: '[]'}
			})(0);
		var cellData = A2(
			_elm_lang$core$Maybe$withDefault,
			defaultCell,
			A2(_elm_lang$core$Array$get, cell, model.data.cell));
		var newFreqData = _elm_lang$core$Array$toList(
			A3(
				_elm_lang$core$Array$slice,
				min,
				max,
				_elm_lang$core$Array$fromList(cellData.frequencyData)));
		var new_cell = A2(_user$project$Pas$asFreqIn, cellData, newFreqData);
		return A3(_user$project$Pas$setCellData, model, cell, new_cell);
	});
var _user$project$Pas$defaultPasCellData = _elm_lang$core$Array$fromList(
	{
		ctor: '::',
		_0: _user$project$Pas$PasCell(0)(0)(0)(
			{
				ctor: '::',
				_0: 0,
				_1: {
					ctor: '::',
					_0: 0,
					_1: {ctor: '[]'}
				}
			})(0)(0)(
			{
				ctor: '::',
				_0: 0,
				_1: {ctor: '[]'}
			})(
			{
				ctor: '::',
				_0: 0,
				_1: {ctor: '[]'}
			})(
			{
				ctor: '::',
				_0: 0,
				_1: {ctor: '[]'}
			})(
			{
				ctor: '::',
				_0: 0,
				_1: {ctor: '[]'}
			})(0),
		_1: {ctor: '[]'}
	});
var _user$project$Pas$PasCvt = function (a) {
	return function (b) {
		return function (c) {
			return function (d) {
				return function (e) {
					return function (f) {
						return function (g) {
							return function (h) {
								return function (i) {
									return function (j) {
										return function (k) {
											return {spk: a, b: b, m: c, fmod_0: d, fmod_1: e, heater_0: f, heater_1: g, enable_0: h, enable_1: i, speaker_0: j, speaker_1: k};
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var _user$project$Pas$Speaker_ = F4(
	function (a, b, c, d) {
		return {vscale: a, voffset: b, center: c, df: d};
	});
var _user$project$Pas$decodeSpeaker = A5(
	_elm_lang$core$Json_Decode$map4,
	_user$project$Pas$Speaker_,
	A2(_elm_lang$core$Json_Decode$field, 'vscale', _user$project$Pas$floatString),
	A2(_elm_lang$core$Json_Decode$field, 'voffset', _user$project$Pas$floatString),
	A2(_elm_lang$core$Json_Decode$field, 'center', _user$project$Pas$intString),
	A2(_elm_lang$core$Json_Decode$field, 'df', _user$project$Pas$intString));
var _user$project$Pas$defaultSpk = A4(_user$project$Pas$Speaker_, '1', '0.5', '1350', '100');
var _user$project$Pas$Heater = F2(
	function (a, b) {
		return {pid: a, sp: b};
	});
var _user$project$Pas$decodeHeater = A3(
	_elm_lang$core$Json_Decode$map2,
	_user$project$Pas$Heater,
	A2(
		_elm_lang$core$Json_Decode$field,
		'pid',
		_elm_lang$core$Json_Decode$array(_elm_lang$core$Json_Decode$string)),
	A2(_elm_lang$core$Json_Decode$field, 'sp', _elm_lang$core$Json_Decode$string));
var _user$project$Pas$decodePasCvt = function (cvt) {
	return A3(
		_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$requiredAt,
		{
			ctor: '::',
			_0: 'ch1',
			_1: {
				ctor: '::',
				_0: 'spk',
				_1: {ctor: '[]'}
			}
		},
		_elm_lang$core$Json_Decode$bool,
		A3(
			_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$requiredAt,
			{
				ctor: '::',
				_0: 'ch0',
				_1: {
					ctor: '::',
					_0: 'spk',
					_1: {ctor: '[]'}
				}
			},
			_elm_lang$core$Json_Decode$bool,
			A3(
				_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$requiredAt,
				{
					ctor: '::',
					_0: 'ch1',
					_1: {
						ctor: '::',
						_0: 'laser_enable',
						_1: {ctor: '[]'}
					}
				},
				_elm_lang$core$Json_Decode$bool,
				A3(
					_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$requiredAt,
					{
						ctor: '::',
						_0: 'ch0',
						_1: {
							ctor: '::',
							_0: 'laser_enable',
							_1: {ctor: '[]'}
						}
					},
					_elm_lang$core$Json_Decode$bool,
					A3(
						_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
						'heater1',
						_user$project$Pas$decodeHeater,
						A3(
							_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
							'heater0',
							_user$project$Pas$decodeHeater,
							A3(
								_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$requiredAt,
								{
									ctor: '::',
									_0: 'ch1',
									_1: {
										ctor: '::',
										_0: 'mod',
										_1: {ctor: '[]'}
									}
								},
								_elm_lang$core$Json_Decode$int,
								A3(
									_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$requiredAt,
									{
										ctor: '::',
										_0: 'ch0',
										_1: {
											ctor: '::',
											_0: 'mod',
											_1: {ctor: '[]'}
										}
									},
									_elm_lang$core$Json_Decode$int,
									A3(
										_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
										'm',
										_elm_lang$core$Json_Decode$list(_elm_lang$core$Json_Decode$float),
										A3(
											_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
											'b',
											_elm_lang$core$Json_Decode$list(_elm_lang$core$Json_Decode$float),
											A3(
												_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
												'spk',
												_user$project$Pas$decodeSpeaker,
												_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$decode(_user$project$Pas$PasCvt))))))))))));
};
var _user$project$Pas$retrievePasCvt = F3(
	function (heading, data, cvt) {
		return _elm_lang$core$Native_Utils.eq(data, '{}') ? cvt : A2(
			_elm_lang$core$Result$withDefault,
			cvt,
			A2(
				_elm_lang$core$Debug$log,
				'pas_cvt',
				A2(
					_elm_lang$core$Json_Decode$decodeString,
					A2(
						_elm_lang$core$Json_Decode$field,
						heading,
						_user$project$Pas$decodePasCvt(cvt)),
					data)));
	});
var _user$project$Pas$defaultHeater = A2(
	_user$project$Pas$Heater,
	_elm_lang$core$Array$fromList(
		{
			ctor: '::',
			_0: '1',
			_1: {
				ctor: '::',
				_0: '0',
				_1: {
					ctor: '::',
					_0: '0',
					_1: {ctor: '[]'}
				}
			}
		}),
	'18');
var _user$project$Pas$defaultCvt = _user$project$Pas$PasCvt(_user$project$Pas$defaultSpk)(
	{
		ctor: '::',
		_0: 0,
		_1: {
			ctor: '::',
			_0: 0,
			_1: {ctor: '[]'}
		}
	})(
	{
		ctor: '::',
		_0: 1,
		_1: {
			ctor: '::',
			_0: 1,
			_1: {ctor: '[]'}
		}
	})(1350)(1350)(_user$project$Pas$defaultHeater)(_user$project$Pas$defaultHeater)(false)(false)(true)(true);
var _user$project$Pas$UpdateSpkFcenter = function (a) {
	return {ctor: 'UpdateSpkFcenter', _0: a};
};
var _user$project$Pas$UpdateSpkDf = function (a) {
	return {ctor: 'UpdateSpkDf', _0: a};
};
var _user$project$Pas$UpdateSpkVoffset = function (a) {
	return {ctor: 'UpdateSpkVoffset', _0: a};
};
var _user$project$Pas$UpdateSpkVscale = function (a) {
	return {ctor: 'UpdateSpkVscale', _0: a};
};
var _user$project$Pas$HandleGeneric = function (a) {
	return {ctor: 'HandleGeneric', _0: a};
};
var _user$project$Pas$UpdateFreq = F2(
	function (a, b) {
		return {ctor: 'UpdateFreq', _0: a, _1: b};
	});
var _user$project$Pas$Speaker = {ctor: 'Speaker'};
var _user$project$Pas$Laser = {ctor: 'Laser'};
var _user$project$Pas$defaultPasData = A2(_user$project$Pas$PasData, _user$project$Pas$defaultPasCellData, _user$project$Pas$Laser);
var _user$project$Pas$init = {cvt: _user$project$Pas$defaultCvt, data: _user$project$Pas$defaultPasData};
var _user$project$Pas$decodeDrive = function () {
	var helper = function (d) {
		return d ? _elm_lang$core$Json_Decode$succeed(_user$project$Pas$Speaker) : _elm_lang$core$Json_Decode$succeed(_user$project$Pas$Laser);
	};
	return A2(_elm_lang$core$Json_Decode$andThen, helper, _elm_lang$core$Json_Decode$bool);
}();
var _user$project$Pas$decodePASData = A3(
	_elm_lang$core$Json_Decode$map2,
	_user$project$Pas$PasData,
	A2(
		_elm_lang$core$Json_Decode$field,
		'CellData',
		_elm_lang$core$Json_Decode$array(_user$project$Pas$decodeCellData)),
	A2(_elm_lang$core$Json_Decode$field, 'Drive', _user$project$Pas$decodeDrive));
var _user$project$Pas$retrievePasData = F3(
	function (head, data, model) {
		var new_model = A2(
			_user$project$Pas$asDataIn,
			model,
			A2(
				_elm_lang$core$Result$withDefault,
				model.data,
				A2(
					_elm_lang$core$Json_Decode$decodeString,
					A2(_elm_lang$core$Json_Decode$field, head, _user$project$Pas$decodePASData),
					data)));
		return new_model;
	});

var _user$project$Main$secondElement = function (_p0) {
	var _p1 = _p0;
	return _p1._1;
};
var _user$project$Main$firstElement = function (_p2) {
	var _p3 = _p2;
	return _p3._0;
};
var _user$project$Main$thirdElement = function (_p4) {
	var _p5 = _p4;
	return _p5._2;
};
var _user$project$Main$min3 = F2(
	function (_p7, _p6) {
		var _p8 = _p7;
		var _p9 = _p6;
		return {
			ctor: '_Tuple3',
			_0: A2(_elm_lang$core$Basics$min, _p8._0, _p9._0),
			_1: A2(_elm_lang$core$Basics$min, _p8._1, _p9._1),
			_2: A2(_elm_lang$core$Basics$min, _p8._2, _p9._2)
		};
	});
var _user$project$Main$max3 = F2(
	function (_p11, _p10) {
		var _p12 = _p11;
		var _p13 = _p10;
		return {
			ctor: '_Tuple3',
			_0: A2(_elm_lang$core$Basics$max, _p12._0, _p13._0),
			_1: A2(_elm_lang$core$Basics$max, _p12._1, _p13._1),
			_2: A2(_elm_lang$core$Basics$max, _p12._2, _p13._2)
		};
	});
var _user$project$Main$findAll = F2(
	function (idx, lists) {
		return A2(
			_elm_lang$core$List$filterMap,
			_elm_community$list_extra$List_Extra$getAt(idx),
			lists);
	});
var _user$project$Main$addDataToList = F3(
	function (size, newData, oldData) {
		return (_elm_lang$core$Native_Utils.cmp(
			_elm_lang$core$List$length(oldData),
			size) > -1) ? A2(
			_elm_lang$core$List$append,
			{
				ctor: '::',
				_0: newData,
				_1: {ctor: '[]'}
			},
			A2(_elm_lang$core$List$take, size - 1, oldData)) : (_elm_lang$core$List$isEmpty(oldData) ? {
			ctor: '::',
			_0: newData,
			_1: {ctor: '[]'}
		} : A2(
			_elm_lang$core$List$append,
			{
				ctor: '::',
				_0: newData,
				_1: {ctor: '[]'}
			},
			oldData));
	});
var _user$project$Main$getNumericField = F2(
	function (reject, num) {
		return _elm_lang$core$Native_Utils.eq(num, reject) ? '' : _elm_lang$core$Basics$toString(num);
	});
var _user$project$Main$toScientific = F2(
	function (x, y) {
		var exp = _elm_lang$core$Basics$floor(
			A2(_elm_lang$core$Basics$logBase, 10, y));
		var num = A2(
			_myrho$elm_round$Round$round,
			x,
			y / Math.pow(
				10,
				_elm_lang$core$Basics$toFloat(exp)));
		var e = (_elm_lang$core$Native_Utils.cmp(exp, 10) < 0) ? A2(
			_elm_lang$core$Basics_ops['++'],
			'0',
			_elm_lang$core$Basics$toString(exp)) : _elm_lang$core$Basics$toString(exp);
		return A2(
			_elm_lang$core$Basics_ops['++'],
			num,
			A2(_elm_lang$core$Basics_ops['++'], 'e', e));
	});
var _user$project$Main$printableNumeric = function (number) {
	return ((_elm_lang$core$Native_Utils.cmp(number, 1.0e-3) > -1) || (_elm_lang$core$Native_Utils.cmp(number, 1000) < 1)) ? A2(_myrho$elm_round$Round$round, 2, number) : A2(_user$project$Main$toScientific, 2, number);
};
var _user$project$Main$convertToFloat = _elm_lang$core$List$map(
	function (a) {
		return _elm_lang$core$Basics$toFloat(a);
	});
var _user$project$Main$getPasTimeData = F3(
	function (model, xstart, dataType) {
		var cell_1 = A2(
			_elm_lang$core$Maybe$withDefault,
			_user$project$Pas$PasCell(0)(0)(0)(
				{
					ctor: '::',
					_0: 0,
					_1: {
						ctor: '::',
						_0: 0,
						_1: {ctor: '[]'}
					}
				})(0)(0)(
				{
					ctor: '::',
					_0: 0,
					_1: {ctor: '[]'}
				})(
				{
					ctor: '::',
					_0: 0,
					_1: {ctor: '[]'}
				})(
				{
					ctor: '::',
					_0: 0,
					_1: {ctor: '[]'}
				})(
				{
					ctor: '::',
					_0: 0,
					_1: {ctor: '[]'}
				})(0),
			A2(_elm_lang$core$Array$get, 1, model.pas.data.cell));
		var cell_0 = A2(
			_elm_lang$core$Maybe$withDefault,
			_user$project$Pas$PasCell(0)(0)(0)(
				{
					ctor: '::',
					_0: 0,
					_1: {
						ctor: '::',
						_0: 0,
						_1: {ctor: '[]'}
					}
				})(0)(0)(
				{
					ctor: '::',
					_0: 0,
					_1: {ctor: '[]'}
				})(
				{
					ctor: '::',
					_0: 0,
					_1: {ctor: '[]'}
				})(
				{
					ctor: '::',
					_0: 0,
					_1: {ctor: '[]'}
				})(
				{
					ctor: '::',
					_0: 0,
					_1: {ctor: '[]'}
				})(0),
			A2(_elm_lang$core$Array$get, 0, model.pas.data.cell));
		var data_in = function () {
			var _p14 = dataType;
			switch (_p14.ctor) {
				case 'MicFreq':
					return {ctor: '_Tuple2', _0: cell_0.frequencyData, _1: cell_1.frequencyData};
				case 'MicTime':
					return {
						ctor: '_Tuple2',
						_0: _user$project$Main$convertToFloat(cell_0.timeData),
						_1: _user$project$Main$convertToFloat(cell_1.timeData)
					};
				default:
					return {
						ctor: '_Tuple2',
						_0: _user$project$Main$convertToFloat(cell_0.laserDiodeData),
						_1: _user$project$Main$convertToFloat(cell_1.laserDiodeData)
					};
			}
		}();
		return A4(
			_elm_lang$core$List$map3,
			F3(
				function (i, a, b) {
					return {ctor: '_Tuple3', _0: xstart + i, _1: a, _2: b};
				}),
			_user$project$Main$convertToFloat(
				A2(
					_elm_lang$core$List$range,
					0,
					_elm_lang$core$List$length(
						_elm_lang$core$Tuple$first(data_in)))),
			_elm_lang$core$Tuple$first(data_in),
			_elm_lang$core$Tuple$second(data_in));
	});
var _user$project$Main$getRingdownData = function (model) {
	var cell_1 = A2(
		_elm_lang$core$Maybe$withDefault,
		_user$project$Crd$CrdsCell(0)(0)(0)(0)(0)(0)(0)(0)(0)(
			{
				ctor: '::',
				_0: {
					ctor: '::',
					_0: 0,
					_1: {ctor: '[]'}
				},
				_1: {ctor: '[]'}
			}),
		A2(_elm_lang$core$Array$get, 1, model.crd.data));
	var raw_data_1 = A2(
		_elm_lang$core$Maybe$withDefault,
		{ctor: '[]'},
		_elm_lang$core$List$head(cell_1.ringdowns));
	var cell_0 = A2(
		_elm_lang$core$Maybe$withDefault,
		_user$project$Crd$CrdsCell(0)(0)(0)(0)(0)(0)(0)(0)(0)(
			{
				ctor: '::',
				_0: {
					ctor: '::',
					_0: 0,
					_1: {ctor: '[]'}
				},
				_1: {ctor: '[]'}
			}),
		A2(_elm_lang$core$Array$get, 0, model.crd.data));
	var raw_data_0 = A2(
		_elm_lang$core$Maybe$withDefault,
		{ctor: '[]'},
		_elm_lang$core$List$head(cell_0.ringdowns));
	return A4(
		_elm_lang$core$List$map3,
		F3(
			function (i, a, b) {
				return {ctor: '_Tuple3', _0: i, _1: a, _2: b};
			}),
		_user$project$Main$convertToFloat(
			A2(
				_elm_lang$core$List$range,
				0,
				_elm_lang$core$List$length(raw_data_0))),
		_user$project$Main$convertToFloat(raw_data_0),
		_user$project$Main$convertToFloat(raw_data_1));
};
var _user$project$Main$dataAxis = _terezka$elm_plot$Plot$customAxis(
	function (summary) {
		return {
			position: _terezka$elm_plot$Plot$closestToZero,
			axisLine: _elm_lang$core$Maybe$Just(
				_terezka$elm_plot$Plot$simpleLine(summary)),
			ticks: {ctor: '[]'},
			labels: {ctor: '[]'},
			flipAnchor: false
		};
	});
var _user$project$Main$plotData = F2(
	function (data, data_index) {
		var plotf = function (coords) {
			return A2(
				_elm_lang$core$List$map,
				function (_p15) {
					var _p16 = _p15;
					return A3(
						_terezka$elm_plot$Plot$dot,
						A2(_terezka$elm_plot$Plot$viewCircle, 2, 'blue'),
						_p16._0,
						_p16._1);
				},
				coords);
		};
		var pdata = A2(
			_elm_lang$core$List$indexedMap,
			F2(
				function (idx, d) {
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Basics$toFloat(idx),
						_1: d
					};
				}),
			A2(_user$project$Main$findAll, data_index, data));
		return A3(
			_terezka$elm_plot$Plot$viewSeriesCustom,
			_elm_lang$core$Native_Utils.update(
				_terezka$elm_plot$Plot$defaultSeriesPlotCustomizations,
				{
					height: 200,
					horizontalAxis: _user$project$Main$dataAxis,
					toDomainLowest: function (y) {
						return A2(_elm_lang$core$Basics$max, y, 0);
					},
					toDomainHighest: function (y) {
						return A2(_elm_lang$core$Basics$min, y, 1500);
					},
					margin: {top: 25, right: 25, bottom: 25, left: 50}
				}),
			{
				ctor: '::',
				_0: _terezka$elm_plot$Plot$line(plotf),
				_1: {ctor: '[]'}
			},
			pdata);
	});
var _user$project$Main$highestRange = F2(
	function (high, y) {
		return A2(_elm_lang$core$Basics$max, high, y);
	});
var _user$project$Main$lowestRange = F2(
	function (low, y) {
		return A2(_elm_lang$core$Basics$min, low, y);
	});
var _user$project$Main$selectData = F2(
	function (selected_, data) {
		return selected_ ? _elm_lang$core$Maybe$Just(data) : _elm_lang$core$Maybe$Nothing;
	});
var _user$project$Main$timeData = F4(
	function (model, range, data, selectData) {
		var cell0 = function (coords) {
			return A2(
				_elm_lang$core$List$map,
				function (_p17) {
					var _p18 = _p17;
					return A3(
						_terezka$elm_plot$Plot$dot,
						A2(_terezka$elm_plot$Plot$viewCircle, 2, 'blue'),
						_p18._0,
						_p18._1);
				},
				coords);
		};
		var cell1 = function (coords) {
			return A2(
				_elm_lang$core$List$map,
				function (_p19) {
					var _p20 = _p19;
					return A3(
						_terezka$elm_plot$Plot$dot,
						A2(_terezka$elm_plot$Plot$viewCircle, 2, 'red'),
						_p20._0,
						_p20._2);
				},
				coords);
		};
		var seriesList = A2(
			_elm_lang$core$List$filterMap,
			function (x) {
				return _elm_lang$core$Tuple$second(x) ? _elm_lang$core$Maybe$Just(
					_elm_lang$core$Tuple$first(x)) : _elm_lang$core$Maybe$Nothing;
			},
			A3(
				_elm_lang$core$List$map2,
				F2(
					function (v0, v1) {
						return {ctor: '_Tuple2', _0: v0, _1: v1};
					}),
				{
					ctor: '::',
					_0: _terezka$elm_plot$Plot$line(cell0),
					_1: {
						ctor: '::',
						_0: _terezka$elm_plot$Plot$line(cell1),
						_1: {ctor: '[]'}
					}
				},
				selectData));
		return A3(
			_terezka$elm_plot$Plot$viewSeriesCustom,
			_elm_lang$core$Native_Utils.update(
				_terezka$elm_plot$Plot$defaultSeriesPlotCustomizations,
				{
					height: 200,
					toRangeLowest: function (y) {
						return range.xmin;
					},
					toRangeHighest: function (y) {
						return range.xmax;
					},
					toDomainLowest: function (y) {
						return range.ymin;
					},
					toDomainHighest: function (y) {
						return range.ymax;
					},
					margin: {top: 25, right: 25, bottom: 25, left: 50}
				}),
			seriesList,
			data);
	});
var _user$project$Main$ringdownTitle = A2(
	_terezka$elm_plot$Plot$viewLabel,
	{
		ctor: '::',
		_0: _elm_lang$svg$Svg_Attributes$fill('#afafaf'),
		_1: {
			ctor: '::',
			_0: _elm_lang$svg$Svg_Attributes$style('text-anchor: end; font-style: italic;'),
			_1: {ctor: '[]'}
		}
	},
	'Ringdown Data');
var _user$project$Main$getHeaterSP = F2(
	function (model, heater) {
		var _p21 = heater;
		switch (_p21) {
			case 0:
				return model.pas.cvt.heater_0.sp;
			case 1:
				return model.pas.cvt.heater_1.sp;
			case 2:
				return model.crd.cvt.heater.sp;
			default:
				return '';
		}
	});
var _user$project$Main$getHeaterPid = F3(
	function (model, heater, pid) {
		var _p22 = heater;
		switch (_p22) {
			case 0:
				return A2(
					_elm_lang$core$Maybe$withDefault,
					'0',
					A2(_elm_lang$core$Array$get, pid, model.pas.cvt.heater_0.pid));
			case 1:
				return A2(
					_elm_lang$core$Maybe$withDefault,
					'0',
					A2(_elm_lang$core$Array$get, pid, model.pas.cvt.heater_1.pid));
			case 2:
				return A2(
					_elm_lang$core$Maybe$withDefault,
					'0',
					A2(_elm_lang$core$Array$get, pid, model.crd.cvt.heater.pid));
			default:
				return '0';
		}
	});
var _user$project$Main$viewMain = function (model) {
	return A2(_user$project$Main$plotData, model.runningData, 0);
};
var _user$project$Main$interlockIcon = function (model) {
	return model.genData.interlock ? A2(
		_debois$elm_mdl$Material_Icon$view,
		'lock_outline',
		{
			ctor: '::',
			_0: _debois$elm_mdl$Material_Icon$size24,
			_1: {
				ctor: '::',
				_0: _debois$elm_mdl$Material_Color$text(
					A2(_debois$elm_mdl$Material_Color$color, _debois$elm_mdl$Material_Color$Green, _debois$elm_mdl$Material_Color$S400)),
				_1: {ctor: '[]'}
			}
		}) : A2(
		_debois$elm_mdl$Material_Icon$view,
		'lock_open',
		{
			ctor: '::',
			_0: _debois$elm_mdl$Material_Icon$size24,
			_1: {
				ctor: '::',
				_0: _debois$elm_mdl$Material_Color$text(
					A2(_debois$elm_mdl$Material_Color$color, _debois$elm_mdl$Material_Color$Red, _debois$elm_mdl$Material_Color$S400)),
				_1: {ctor: '[]'}
			}
		});
};
var _user$project$Main$viewFooter = function (model) {
	return A2(
		_debois$elm_mdl$Material_Footer$mini,
		{
			ctor: '::',
			_0: A2(_debois$elm_mdl$Material_Options$css, 'position', 'fixed'),
			_1: {
				ctor: '::',
				_0: A2(_debois$elm_mdl$Material_Options$css, 'bottom', '0px'),
				_1: {
					ctor: '::',
					_0: A2(_debois$elm_mdl$Material_Options$css, 'width', '100%'),
					_1: {
						ctor: '::',
						_0: A2(_debois$elm_mdl$Material_Options$css, 'padding', '5px'),
						_1: {
							ctor: '::',
							_0: _debois$elm_mdl$Material_Color$background(
								A2(_debois$elm_mdl$Material_Color$color, _debois$elm_mdl$Material_Color$LightBlue, _debois$elm_mdl$Material_Color$S400)),
							_1: {ctor: '[]'}
						}
					}
				}
			}
		},
		{
			left: A2(
				_debois$elm_mdl$Material_Footer$left,
				{ctor: '[]'},
				{
					ctor: '::',
					_0: A2(
						_debois$elm_mdl$Material_Footer$logo,
						{ctor: '[]'},
						{
							ctor: '::',
							_0: _debois$elm_mdl$Material_Footer$html(
								_elm_lang$html$Html$text(
									A2(_elm_lang$core$Basics_ops['++'], 'Time: ', model.genData.time))),
							_1: {ctor: '[]'}
						}),
					_1: {ctor: '[]'}
				}),
			right: A2(
				_debois$elm_mdl$Material_Footer$right,
				{ctor: '[]'},
				{
					ctor: '::',
					_0: A2(
						_debois$elm_mdl$Material_Footer$logo,
						{ctor: '[]'},
						{
							ctor: '::',
							_0: _debois$elm_mdl$Material_Footer$html(
								A2(
									_debois$elm_mdl$Material_Options$span,
									{
										ctor: '::',
										_0: _debois$elm_mdl$Material_Badge$add(
											_elm_lang$core$Basics$toString(
												A2(
													_elm_lang$core$Maybe$withDefault,
													0,
													A2(_elm_community$list_extra$List_Extra$getAt, 0, model.msgs)))),
										_1: {ctor: '[]'}
									},
									{ctor: '[]'})),
							_1: {ctor: '[]'}
						}),
					_1: {
						ctor: '::',
						_0: A2(
							_debois$elm_mdl$Material_Footer$logo,
							{ctor: '[]'},
							{
								ctor: '::',
								_0: _debois$elm_mdl$Material_Footer$html(
									_user$project$Main$interlockIcon(model)),
								_1: {ctor: '[]'}
							}),
						_1: {ctor: '[]'}
					}
				})
		});
};
var _user$project$Main$pageHeader = function (model) {
	return {
		ctor: '::',
		_0: A2(
			_debois$elm_mdl$Material_Layout$row,
			{
				ctor: '::',
				_0: _debois$elm_mdl$Material_Options$nop,
				_1: {
					ctor: '::',
					_0: A2(_debois$elm_mdl$Material_Options$css, 'transition', 'height 333ms ease-in-out 0s'),
					_1: {ctor: '[]'}
				}
			},
			{
				ctor: '::',
				_0: A2(
					_debois$elm_mdl$Material_Layout$title,
					{ctor: '[]'},
					{
						ctor: '::',
						_0: _elm_lang$html$Html$text('NOAA SOAP'),
						_1: {ctor: '[]'}
					}),
				_1: {
					ctor: '::',
					_0: _debois$elm_mdl$Material_Layout$spacer,
					_1: {
						ctor: '::',
						_0: A2(
							_debois$elm_mdl$Material_Layout$navigation,
							{ctor: '[]'},
							{
								ctor: '::',
								_0: A2(
									_debois$elm_mdl$Material_Layout$link,
									{ctor: '[]'},
									{ctor: '[]'}),
								_1: {
									ctor: '::',
									_0: A2(
										_debois$elm_mdl$Material_Layout$link,
										{
											ctor: '::',
											_0: _debois$elm_mdl$Material_Layout$href('https://esrl.noaa.gov/csd'),
											_1: {ctor: '[]'}
										},
										{
											ctor: '::',
											_0: A2(
												_elm_lang$html$Html$span,
												{ctor: '[]'},
												{
													ctor: '::',
													_0: _elm_lang$html$Html$text('noaa'),
													_1: {ctor: '[]'}
												}),
											_1: {ctor: '[]'}
										}),
									_1: {ctor: '[]'}
								}
							}),
						_1: {ctor: '[]'}
					}
				}
			}),
		_1: {ctor: '[]'}
	};
};
var _user$project$Main$setCvt = F2(
	function (cvt, mdl) {
		return _elm_lang$core$Native_Utils.update(
			mdl,
			{cvt: cvt});
	});
var _user$project$Main$asCvtIn = _elm_lang$core$Basics$flip(_user$project$Main$setCvt);
var _user$project$Main$asPasIn = F2(
	function (model, pas) {
		return _elm_lang$core$Native_Utils.update(
			model,
			{pas: pas});
	});
var _user$project$Main$asVaisalaIn = F2(
	function (model, vaisala) {
		return _elm_lang$core$Native_Utils.update(
			model,
			{vaisalas: vaisala});
	});
var _user$project$Main$asPptIn = F2(
	function (model, ppt) {
		return _elm_lang$core$Native_Utils.update(
			model,
			{ppts: ppt});
	});
var _user$project$Main$asAlicatIn = F2(
	function (model, alicat) {
		return _elm_lang$core$Native_Utils.update(
			model,
			{alicats: alicat});
	});
var _user$project$Main$asCrdIn = F2(
	function (model, crd) {
		return _elm_lang$core$Native_Utils.update(
			model,
			{crd: crd});
	});
var _user$project$Main$asNetworkIn = F2(
	function (model, network) {
		return _elm_lang$core$Native_Utils.update(
			model,
			{network: network});
	});
var _user$project$Main$asDataIn = F2(
	function (model, data) {
		return _elm_lang$core$Native_Utils.update(
			model,
			{genData: data});
	});
var _user$project$Main$asCrdRunningDataIn = F2(
	function (model, ldata) {
		return _elm_lang$core$Native_Utils.update(
			model,
			{crdRunningData: ldata});
	});
var _user$project$Main$asRunningDataIn = F2(
	function (model, ldata) {
		return _elm_lang$core$Native_Utils.update(
			model,
			{runningData: ldata});
	});
var _user$project$Main$getPasCvt = function (model) {
	return model.pas.cvt;
};
var _user$project$Main$getCrdCvt = function (model) {
	return model.crd.cvt;
};
var _user$project$Main$webService = 'soap';
var _user$project$Main$RangeData = F4(
	function (a, b, c, d) {
		return {xmin: a, xmax: b, ymin: c, ymax: d};
	});
var _user$project$Main$defaultModelData = {
	pas: _user$project$Pas$init,
	crd: _user$project$Crd$init,
	genData: _user$project$GcrdTypes$defaultData,
	cvt: _user$project$GcrdTypes$defaultCvtData,
	save: true,
	selectedTab: 0,
	dt: _Bogdanp$elm_time$Time_DateTime$fromTuple(
		{ctor: '_Tuple7', _0: 0, _1: 0, _2: 0, _3: 0, _4: 0, _5: 0, _6: 0}),
	mdl: _debois$elm_mdl$Material$model,
	network: {ip: '192.168.172.123', port_: '8001', service: 'soap'},
	runningData: {
		ctor: '::',
		_0: {ctor: '[]'},
		_1: {ctor: '[]'}
	},
	crdRunningData: {
		ctor: '::',
		_0: {ctor: '[]'},
		_1: {ctor: '[]'}
	},
	currentMsgList: {ctor: '[]'},
	alicats: _user$project$Devices_Alicat$init,
	vaisalas: _user$project$Devices_Vaisala$init,
	ppts: _user$project$Devices_Ppt$init,
	msgs: {
		ctor: '::',
		_0: 0,
		_1: {
			ctor: '::',
			_0: 0,
			_1: {
				ctor: '::',
				_0: 0,
				_1: {ctor: '[]'}
			}
		}
	},
	pasPlotData: {
		ctor: '::',
		_0: true,
		_1: {
			ctor: '::',
			_0: true,
			_1: {
				ctor: '::',
				_0: true,
				_1: {ctor: '[]'}
			}
		}
	},
	pasRange: A4(_user$project$Main$RangeData, 0, 1000, 0, 1500),
	crdPlotData: {
		ctor: '::',
		_0: true,
		_1: {
			ctor: '::',
			_0: true,
			_1: {
				ctor: '::',
				_0: true,
				_1: {ctor: '[]'}
			}
		}
	},
	crdRange: A4(_user$project$Main$RangeData, 0, 1000, 0, 1500)
};
var _user$project$Main$init = function () {
	var model = _user$project$Main$defaultModelData;
	return {ctor: '_Tuple2', _0: model, _1: _elm_lang$core$Platform_Cmd$none};
}();
var _user$project$Main$Model = function (a) {
	return function (b) {
		return function (c) {
			return function (d) {
				return function (e) {
					return function (f) {
						return function (g) {
							return function (h) {
								return function (i) {
									return function (j) {
										return function (k) {
											return function (l) {
												return function (m) {
													return function (n) {
														return function (o) {
															return function (p) {
																return function (q) {
																	return function (r) {
																		return function (s) {
																			return function (t) {
																				return {pas: a, crd: b, genData: c, cvt: d, save: e, selectedTab: f, dt: g, mdl: h, network: i, runningData: j, crdRunningData: k, currentMsgList: l, alicats: m, vaisalas: n, ppts: o, msgs: p, pasPlotData: q, pasRange: r, crdPlotData: s, crdRange: t};
																			};
																		};
																	};
																};
															};
														};
													};
												};
											};
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var _user$project$Main$Pas1Heater = {ctor: 'Pas1Heater'};
var _user$project$Main$Pas0Heater = {ctor: 'Pas0Heater'};
var _user$project$Main$CrdHeater = {ctor: 'CrdHeater'};
var _user$project$Main$RunningData = {ctor: 'RunningData'};
var _user$project$Main$ResetSequence = {ctor: 'ResetSequence'};
var _user$project$Main$SequenceState = {ctor: 'SequenceState'};
var _user$project$Main$ToggleCrdPlot = function (a) {
	return {ctor: 'ToggleCrdPlot', _0: a};
};
var _user$project$Main$UpdateCrdScaling = {ctor: 'UpdateCrdScaling'};
var _user$project$Main$UpdateCrdRange = F2(
	function (a, b) {
		return {ctor: 'UpdateCrdRange', _0: a, _1: b};
	});
var _user$project$Main$UpdatePasScaling = {ctor: 'UpdatePasScaling'};
var _user$project$Main$UpdatePasRange = F2(
	function (a, b) {
		return {ctor: 'UpdatePasRange', _0: a, _1: b};
	});
var _user$project$Main$TogglePasPlot = function (a) {
	return {ctor: 'TogglePasPlot', _0: a};
};
var _user$project$Main$SyncTime = {ctor: 'SyncTime'};
var _user$project$Main$UpdateTime = function (a) {
	return {ctor: 'UpdateTime', _0: a};
};
var _user$project$Main$getCurrentTime = A2(_elm_lang$core$Task$perform, _user$project$Main$UpdateTime, _elm_lang$core$Time$now);
var _user$project$Main$SendDevSP = function (a) {
	return {ctor: 'SendDevSP', _0: a};
};
var _user$project$Main$UpdateDevSP = F2(
	function (a, b) {
		return {ctor: 'UpdateDevSP', _0: a, _1: b};
	});
var _user$project$Main$UpdateFanVoltage = function (a) {
	return {ctor: 'UpdateFanVoltage', _0: a};
};
var _user$project$Main$ToggleFan = {ctor: 'ToggleFan'};
var _user$project$Main$ClearMessages = {ctor: 'ClearMessages'};
var _user$project$Main$TogglePasLaserPower = function (a) {
	return {ctor: 'TogglePasLaserPower', _0: a};
};
var _user$project$Main$ToggleCrdPower = {ctor: 'ToggleCrdPower'};
var _user$project$Main$UpdateHeaterSP = F2(
	function (a, b) {
		return {ctor: 'UpdateHeaterSP', _0: a, _1: b};
	});
var _user$project$Main$SendCrdSampleRate = {ctor: 'SendCrdSampleRate'};
var _user$project$Main$SendCrdFrequency = {ctor: 'SendCrdFrequency'};
var _user$project$Main$SendModulation = function (a) {
	return {ctor: 'SendModulation', _0: a};
};
var _user$project$Main$UpdateMod1 = function (a) {
	return {ctor: 'UpdateMod1', _0: a};
};
var _user$project$Main$UpdateMod0 = function (a) {
	return {ctor: 'UpdateMod0', _0: a};
};
var _user$project$Main$UpdateChirp = {ctor: 'UpdateChirp'};
var _user$project$Main$SendSpkVoltage = {ctor: 'SendSpkVoltage'};
var _user$project$Main$ToggleSpeaker = function (a) {
	return {ctor: 'ToggleSpeaker', _0: a};
};
var _user$project$Main$HandleGeneric = function (a) {
	return {ctor: 'HandleGeneric', _0: a};
};
var _user$project$Main$sendNewTime = F2(
	function (t, model) {
		var lvTime = (t - _Bogdanp$elm_time$Time_DateTime$toTimestamp(
			_Bogdanp$elm_time$Time_DateTime$dateTime(
				_elm_lang$core$Native_Utils.update(
					_Bogdanp$elm_time$Time_DateTime$zero,
					{year: 1904})))) / 1000;
		return A2(
			_elm_lang$http$Http$send,
			_user$project$Main$HandleGeneric,
			_elm_lang$http$Http$getString(
				A2(
					_elm_lang$core$Basics_ops['++'],
					_user$project$Network$buildAddress(model.network),
					A2(
						_elm_lang$core$Basics_ops['++'],
						'time?t=',
						_elm_lang$core$Basics$toString(lvTime)))));
	});
var _user$project$Main$changeSequenceState = F2(
	function (state, model) {
		return A2(
			_elm_lang$http$Http$send,
			_user$project$Main$HandleGeneric,
			_elm_lang$http$Http$getString(
				A2(
					_elm_lang$core$Basics_ops['++'],
					_user$project$Network$buildAddress(model.network),
					A2(_elm_lang$core$Basics_ops['++'], 'SequenceState?st=', state))));
	});
var _user$project$Main$toggleSave = function (model) {
	var s = model.save ? '1' : '0';
	return A2(
		_elm_lang$http$Http$send,
		_user$project$Main$HandleGeneric,
		_elm_lang$http$Http$getString(
			A2(
				_elm_lang$core$Basics_ops['++'],
				_user$project$Network$buildAddress(model.network),
				A2(_elm_lang$core$Basics_ops['++'], 'SaveMain?save_=', s))));
};
var _user$project$Main$sendDevSp = F3(
	function (idx, dev, model) {
		var sp = A2(_elm_lang$core$Maybe$withDefault, '0', dev.sp);
		return A2(
			_elm_lang$http$Http$send,
			_user$project$Main$HandleGeneric,
			_elm_lang$http$Http$getString(
				A2(
					_elm_lang$core$Basics_ops['++'],
					_user$project$Network$buildAddress(model.network),
					A2(
						_elm_lang$core$Basics_ops['++'],
						'UpdateDevSP?sp=',
						A2(
							_elm_lang$core$Basics_ops['++'],
							sp,
							A2(_elm_lang$core$Basics_ops['++'], '&idx=', idx))))));
	});
var _user$project$Main$updateWaveforms = function (model) {
	return A2(
		_elm_lang$http$Http$send,
		_user$project$Main$HandleGeneric,
		_elm_lang$http$Http$getString(
			A2(
				_elm_lang$core$Basics_ops['++'],
				_user$project$Network$buildAddress(model.network),
				A2(
					_elm_lang$core$Basics_ops['++'],
					'PAS/UpdateSerializeWvfm?page=',
					_elm_lang$core$Basics$toString(model.selectedTab)))));
};
var _user$project$Main$sendFanVoltage = function (model) {
	var val = _elm_lang$core$Basics$toString(model.cvt.fan_voltage);
	return A2(
		_elm_lang$http$Http$send,
		_user$project$Main$HandleGeneric,
		_elm_lang$http$Http$getString(
			A2(
				_elm_lang$core$Basics_ops['++'],
				_user$project$Network$buildAddress(model.network),
				A2(_elm_lang$core$Basics_ops['++'], 'UpdateFanVoltage?voltage=', val))));
};
var _user$project$Main$setCrdRate = function (model) {
	var s = _elm_lang$core$Basics$toString(model.crd.cvt.dc);
	return A2(
		_elm_lang$http$Http$send,
		_user$project$Main$HandleGeneric,
		_elm_lang$http$Http$getString(
			A2(
				_elm_lang$core$Basics_ops['++'],
				_user$project$Network$buildAddress(model.network),
				A2(_elm_lang$core$Basics_ops['++'], 'CRD/samp_per_cycle?samp_cycle=', s))));
};
var _user$project$Main$setCrdFrequency = function (model) {
	var f = _elm_lang$core$Basics$toString(model.crd.cvt.rate);
	return A2(
		_elm_lang$http$Http$send,
		_user$project$Main$HandleGeneric,
		_elm_lang$http$Http$getString(
			A2(
				_elm_lang$core$Basics_ops['++'],
				_user$project$Network$buildAddress(model.network),
				A2(_elm_lang$core$Basics_ops['++'], 'CRD/laser_rep_rate?RepRate=', f))));
};
var _user$project$Main$sendChirp = function (model) {
	var center = _elm_lang$core$Basics$toString(model.pas.cvt.spk.center);
	var df = _elm_lang$core$Basics$toString(model.pas.cvt.spk.df);
	return A2(
		_elm_lang$http$Http$send,
		_user$project$Main$HandleGeneric,
		_elm_lang$http$Http$getString(
			A2(
				_elm_lang$core$Basics_ops['++'],
				_user$project$Network$buildAddress(model.network),
				A2(
					_elm_lang$core$Basics_ops['++'],
					'PAS/chirp?fcenter=',
					A2(
						_elm_lang$core$Basics_ops['++'],
						center,
						A2(_elm_lang$core$Basics_ops['++'], '&df=', df))))));
};
var _user$project$Main$toggleSpk = F3(
	function (model, val, cell) {
		return A2(
			_elm_lang$http$Http$send,
			_user$project$Main$HandleGeneric,
			_elm_lang$http$Http$getString(
				A2(
					_elm_lang$core$Basics_ops['++'],
					_user$project$Network$buildAddress(model.network),
					A2(
						_elm_lang$core$Basics_ops['++'],
						'PAS/SpeakerState?state=',
						A2(
							_elm_lang$core$Basics_ops['++'],
							val,
							A2(_elm_lang$core$Basics_ops['++'], '&cell=', cell))))));
	});
var _user$project$Main$sendSpkVoltage = function (model) {
	return A2(
		_elm_lang$http$Http$send,
		_user$project$Main$HandleGeneric,
		_elm_lang$http$Http$getString(
			A2(
				_elm_lang$core$Basics_ops['++'],
				_user$project$Network$buildAddress(model.network),
				A2(
					_elm_lang$core$Basics_ops['++'],
					'PAS/spk_voltage?Vscale=',
					A2(
						_elm_lang$core$Basics_ops['++'],
						model.pas.cvt.spk.vscale,
						A2(_elm_lang$core$Basics_ops['++'], '&Voffset=', model.pas.cvt.spk.voffset))))));
};
var _user$project$Main$setCellFrequency = F3(
	function (model, cell, frequency) {
		return A2(
			_elm_lang$http$Http$send,
			_user$project$Main$HandleGeneric,
			_elm_lang$http$Http$getString(
				A2(
					_elm_lang$core$Basics_ops['++'],
					_user$project$Network$buildAddress(model.network),
					A2(
						_elm_lang$core$Basics_ops['++'],
						'PAS/mod_frequency?fmod=',
						A2(
							_elm_lang$core$Basics_ops['++'],
							frequency,
							A2(_elm_lang$core$Basics_ops['++'], '&cell=', cell))))));
	});
var _user$project$Main$toggleUV = F2(
	function (model, val) {
		return A2(
			_elm_lang$http$Http$send,
			_user$project$Main$HandleGeneric,
			_elm_lang$http$Http$getString(
				A2(
					_elm_lang$core$Basics_ops['++'],
					_user$project$Network$buildAddress(model.network),
					A2(_elm_lang$core$Basics_ops['++'], 'ToggleUVLamp?lamp=', val))));
	});
var _user$project$Main$toggleO2 = F2(
	function (model, val) {
		return A2(
			_elm_lang$http$Http$send,
			_user$project$Main$HandleGeneric,
			_elm_lang$http$Http$getString(
				A2(
					_elm_lang$core$Basics_ops['++'],
					_user$project$Network$buildAddress(model.network),
					A2(_elm_lang$core$Basics_ops['++'], 'ToggleO2?valve=', val))));
	});
var _user$project$Main$toggleO3 = F2(
	function (model, val) {
		return A2(
			_elm_lang$http$Http$send,
			_user$project$Main$HandleGeneric,
			_elm_lang$http$Http$getString(
				A2(
					_elm_lang$core$Basics_ops['++'],
					_user$project$Network$buildAddress(model.network),
					A2(_elm_lang$core$Basics_ops['++'], 'ToggleO3?valve=', val))));
	});
var _user$project$Main$toggleFan = F2(
	function (model, val) {
		return A2(
			_elm_lang$http$Http$send,
			_user$project$Main$HandleGeneric,
			_elm_lang$http$Http$getString(
				A2(
					_elm_lang$core$Basics_ops['++'],
					_user$project$Network$buildAddress(model.network),
					A2(_elm_lang$core$Basics_ops['++'], 'ToggleFan?enable=', val))));
	});
var _user$project$Main$toggleFilter = function (model) {
	return A2(
		_elm_lang$http$Http$send,
		_user$project$Main$HandleGeneric,
		_elm_lang$http$Http$getString(
			A2(
				_elm_lang$core$Basics_ops['++'],
				_user$project$Network$buildAddress(model.network),
				A2(
					_elm_lang$core$Basics_ops['++'],
					'ToggleFilter?cell=',
					_elm_lang$core$Basics$toString(model.cvt.filter.pos)))));
};
var _user$project$Main$togglePump = function (model) {
	var val = model.cvt.pump ? '1' : '0';
	return A2(
		_elm_lang$http$Http$send,
		_user$project$Main$HandleGeneric,
		_elm_lang$http$Http$getString(
			A2(
				_elm_lang$core$Basics_ops['++'],
				_user$project$Network$buildAddress(model.network),
				A2(_elm_lang$core$Basics_ops['++'], 'TogglePump?Pump=', val))));
};
var _user$project$Main$toggleCrdPower = F2(
	function (model, val) {
		return A2(
			_elm_lang$http$Http$send,
			_user$project$Main$HandleGeneric,
			_elm_lang$http$Http$getString(
				A2(
					_elm_lang$core$Basics_ops['++'],
					_user$project$Network$buildAddress(model.network),
					A2(_elm_lang$core$Basics_ops['++'], 'CRD/cLaserState?state=', val))));
	});
var _user$project$Main$togglePasLaserPower = F3(
	function (model, val, cell) {
		return A2(
			_elm_lang$http$Http$send,
			_user$project$Main$HandleGeneric,
			_elm_lang$http$Http$getString(
				A2(
					_elm_lang$core$Basics_ops['++'],
					_user$project$Network$buildAddress(model.network),
					A2(
						_elm_lang$core$Basics_ops['++'],
						'PAS/pLaserState?state=',
						A2(
							_elm_lang$core$Basics_ops['++'],
							val,
							A2(_elm_lang$core$Basics_ops['++'], '&cell=', cell))))));
	});
var _user$project$Main$shutdownSystem = function (model) {
	return A2(
		_elm_lang$http$Http$send,
		_user$project$Main$HandleGeneric,
		_elm_lang$http$Http$getString(
			A2(
				_elm_lang$core$Basics_ops['++'],
				_user$project$Network$buildAddress(model.network),
				'Shutdown')));
};
var _user$project$Main$InitializeNetwork = function (a) {
	return {ctor: 'InitializeNetwork', _0: a};
};
var _user$project$Main$GetData = function (a) {
	return {ctor: 'GetData', _0: a};
};
var _user$project$Main$getData = function (model) {
	return A2(
		_elm_lang$http$Http$send,
		_user$project$Main$GetData,
		_elm_lang$http$Http$getString(
			A2(
				_elm_lang$core$Basics_ops['++'],
				_user$project$Network$buildAddress(model.network),
				'data')));
};
var _user$project$Main$GetCVT = function (a) {
	return {ctor: 'GetCVT', _0: a};
};
var _user$project$Main$getCvtData = F2(
	function (model, force) {
		return A2(
			_elm_lang$http$Http$send,
			_user$project$Main$GetCVT,
			_elm_lang$http$Http$getString(
				A2(
					_elm_lang$core$Basics_ops['++'],
					_user$project$Network$buildAddress(model.network),
					A2(_elm_lang$core$Basics_ops['++'], 'cvt?force=', force))));
	});
var _user$project$Main$ToggleUSB = {ctor: 'ToggleUSB'};
var _user$project$Main$ToggleUVLamp = {ctor: 'ToggleUVLamp'};
var _user$project$Main$TogglePump = {ctor: 'TogglePump'};
var _user$project$Main$ToggleFilter = {ctor: 'ToggleFilter'};
var _user$project$Main$ToggleO2 = {ctor: 'ToggleO2'};
var _user$project$Main$ToggleO3 = {ctor: 'ToggleO3'};
var _user$project$Main$Crd = function (a) {
	return {ctor: 'Crd', _0: a};
};
var _user$project$Main$Pas = function (a) {
	return {ctor: 'Pas', _0: a};
};
var _user$project$Main$StopServer = {ctor: 'StopServer'};
var _user$project$Main$SaveData = {ctor: 'SaveData'};
var _user$project$Main$Network = function (a) {
	return {ctor: 'Network', _0: a};
};
var _user$project$Main$CheckData = function (a) {
	return {ctor: 'CheckData', _0: a};
};
var _user$project$Main$ForceCvtCheck = {ctor: 'ForceCvtCheck'};
var _user$project$Main$CheckCvtData = function (a) {
	return {ctor: 'CheckCvtData', _0: a};
};
var _user$project$Main$subscriptions = function (model) {
	return _elm_lang$core$Platform_Sub$batch(
		{
			ctor: '::',
			_0: A2(_elm_lang$core$Time$every, _elm_lang$core$Time$second, _user$project$Main$CheckCvtData),
			_1: {
				ctor: '::',
				_0: _user$project$Network$initPort(_user$project$Main$InitializeNetwork),
				_1: {
					ctor: '::',
					_0: A2(_elm_lang$core$Time$every, _elm_lang$core$Time$second, _user$project$Main$CheckData),
					_1: {ctor: '[]'}
				}
			}
		});
};
var _user$project$Main$SelectTab = function (a) {
	return {ctor: 'SelectTab', _0: a};
};
var _user$project$Main$Mdl = function (a) {
	return {ctor: 'Mdl', _0: a};
};
var _user$project$Main$viewAux = function (model) {
	return A2(
		_debois$elm_mdl$Material_Grid$grid,
		{ctor: '[]'},
		A2(
			_elm_lang$core$Basics_ops['++'],
			{
				ctor: '::',
				_0: A2(
					_debois$elm_mdl$Material_Grid$cell,
					{
						ctor: '::',
						_0: A2(_debois$elm_mdl$Material_Grid$size, _debois$elm_mdl$Material_Grid$All, 12),
						_1: {ctor: '[]'}
					},
					{
						ctor: '::',
						_0: A3(
							_debois$elm_mdl$Material_Options$styled,
							_elm_lang$html$Html$p,
							{
								ctor: '::',
								_0: _debois$elm_mdl$Material_Typography$headline,
								_1: {ctor: '[]'}
							},
							{
								ctor: '::',
								_0: _elm_lang$html$Html$text('Heater Controls'),
								_1: {ctor: '[]'}
							}),
						_1: {ctor: '[]'}
					}),
				_1: {ctor: '[]'}
			},
			A2(
				_elm_lang$core$Basics_ops['++'],
				A2(
					_elm_lang$core$List$indexedMap,
					F2(
						function (index, heater) {
							return A2(
								_debois$elm_mdl$Material_Grid$cell,
								{
									ctor: '::',
									_0: A2(_debois$elm_mdl$Material_Grid$size, _debois$elm_mdl$Material_Grid$All, 2),
									_1: {ctor: '[]'}
								},
								{
									ctor: '::',
									_0: A2(
										_debois$elm_mdl$Material_Options$div,
										{
											ctor: '::',
											_0: A2(_debois$elm_mdl$Material_Options$css, 'border-style', 'solid'),
											_1: {
												ctor: '::',
												_0: A2(_debois$elm_mdl$Material_Options$css, 'border-width', '1px'),
												_1: {
													ctor: '::',
													_0: A2(_debois$elm_mdl$Material_Options$css, 'border-radius', '5px'),
													_1: {
														ctor: '::',
														_0: A2(_debois$elm_mdl$Material_Options$css, 'padding-left', '10px'),
														_1: {
															ctor: '::',
															_0: A2(_debois$elm_mdl$Material_Options$css, 'padding-top', '10px'),
															_1: {ctor: '[]'}
														}
													}
												}
											}
										},
										{
											ctor: '::',
											_0: A3(
												_debois$elm_mdl$Material_Options$styled,
												_elm_lang$html$Html$p,
												{
													ctor: '::',
													_0: _debois$elm_mdl$Material_Typography$title,
													_1: {ctor: '[]'}
												},
												{
													ctor: '::',
													_0: _elm_lang$html$Html$text(heater),
													_1: {ctor: '[]'}
												}),
											_1: {
												ctor: '::',
												_0: A5(
													_debois$elm_mdl$Material_Toggles$switch,
													_user$project$Main$Mdl,
													{
														ctor: '::',
														_0: 18 + (2 * index),
														_1: {ctor: '[]'}
													},
													model.mdl,
													{
														ctor: '::',
														_0: _debois$elm_mdl$Material_Toggles$ripple,
														_1: {ctor: '[]'}
													},
													{
														ctor: '::',
														_0: _elm_lang$html$Html$text('Power'),
														_1: {ctor: '[]'}
													}),
												_1: {
													ctor: '::',
													_0: A5(
														_debois$elm_mdl$Material_Textfield$render,
														_user$project$Main$Mdl,
														{
															ctor: '::',
															_0: 19 + (2 * index),
															_1: {ctor: '[]'}
														},
														model.mdl,
														{
															ctor: '::',
															_0: _debois$elm_mdl$Material_Textfield$floatingLabel,
															_1: {
																ctor: '::',
																_0: A2(_debois$elm_mdl$Material_Options$css, 'width', '125px'),
																_1: {
																	ctor: '::',
																	_0: _debois$elm_mdl$Material_Textfield$maxlength(15),
																	_1: {
																		ctor: '::',
																		_0: _debois$elm_mdl$Material_Textfield$value(
																			A2(_user$project$Main$getHeaterSP, model, index)),
																		_1: {
																			ctor: '::',
																			_0: _debois$elm_mdl$Material_Textfield$label('Setpoint'),
																			_1: {ctor: '[]'}
																		}
																	}
																}
															}
														},
														{ctor: '[]'}),
													_1: {
														ctor: '::',
														_0: A5(
															_debois$elm_mdl$Material_Textfield$render,
															_user$project$Main$Mdl,
															{
																ctor: '::',
																_0: 20 + index,
																_1: {ctor: '[]'}
															},
															model.mdl,
															{
																ctor: '::',
																_0: _debois$elm_mdl$Material_Textfield$floatingLabel,
																_1: {
																	ctor: '::',
																	_0: A2(_debois$elm_mdl$Material_Options$css, 'width', '125px'),
																	_1: {
																		ctor: '::',
																		_0: _debois$elm_mdl$Material_Textfield$maxlength(15),
																		_1: {
																			ctor: '::',
																			_0: _debois$elm_mdl$Material_Textfield$value(
																				A3(_user$project$Main$getHeaterPid, model, index, 0)),
																			_1: {
																				ctor: '::',
																				_0: _debois$elm_mdl$Material_Textfield$label('P'),
																				_1: {ctor: '[]'}
																			}
																		}
																	}
																}
															},
															{ctor: '[]'}),
														_1: {
															ctor: '::',
															_0: A5(
																_debois$elm_mdl$Material_Textfield$render,
																_user$project$Main$Mdl,
																{
																	ctor: '::',
																	_0: 21 + index,
																	_1: {ctor: '[]'}
																},
																model.mdl,
																{
																	ctor: '::',
																	_0: _debois$elm_mdl$Material_Textfield$floatingLabel,
																	_1: {
																		ctor: '::',
																		_0: A2(_debois$elm_mdl$Material_Options$css, 'width', '125px'),
																		_1: {
																			ctor: '::',
																			_0: _debois$elm_mdl$Material_Textfield$maxlength(15),
																			_1: {
																				ctor: '::',
																				_0: _debois$elm_mdl$Material_Textfield$value(
																					A3(_user$project$Main$getHeaterPid, model, index, 1)),
																				_1: {
																					ctor: '::',
																					_0: _debois$elm_mdl$Material_Textfield$label('I'),
																					_1: {ctor: '[]'}
																				}
																			}
																		}
																	}
																},
																{ctor: '[]'}),
															_1: {
																ctor: '::',
																_0: A5(
																	_debois$elm_mdl$Material_Textfield$render,
																	_user$project$Main$Mdl,
																	{
																		ctor: '::',
																		_0: 22 + index,
																		_1: {ctor: '[]'}
																	},
																	model.mdl,
																	{
																		ctor: '::',
																		_0: _debois$elm_mdl$Material_Textfield$floatingLabel,
																		_1: {
																			ctor: '::',
																			_0: A2(_debois$elm_mdl$Material_Options$css, 'width', '125px'),
																			_1: {
																				ctor: '::',
																				_0: _debois$elm_mdl$Material_Textfield$maxlength(15),
																				_1: {
																					ctor: '::',
																					_0: _debois$elm_mdl$Material_Textfield$value(
																						A3(_user$project$Main$getHeaterPid, model, index, 2)),
																					_1: {
																						ctor: '::',
																						_0: _debois$elm_mdl$Material_Textfield$label('D'),
																						_1: {ctor: '[]'}
																					}
																				}
																			}
																		}
																	},
																	{ctor: '[]'}),
																_1: {ctor: '[]'}
															}
														}
													}
												}
											}
										}),
									_1: {ctor: '[]'}
								});
						}),
					{
						ctor: '::',
						_0: 'PAS 0',
						_1: {
							ctor: '::',
							_0: 'PAS 1',
							_1: {
								ctor: '::',
								_0: 'CRD',
								_1: {ctor: '[]'}
							}
						}
					}),
				A2(
					_elm_lang$core$Basics_ops['++'],
					{
						ctor: '::',
						_0: A2(
							_debois$elm_mdl$Material_Grid$cell,
							{
								ctor: '::',
								_0: A2(_debois$elm_mdl$Material_Grid$size, _debois$elm_mdl$Material_Grid$All, 12),
								_1: {ctor: '[]'}
							},
							{
								ctor: '::',
								_0: A3(
									_debois$elm_mdl$Material_Options$styled,
									_elm_lang$html$Html$p,
									{
										ctor: '::',
										_0: _debois$elm_mdl$Material_Typography$headline,
										_1: {ctor: '[]'}
									},
									{
										ctor: '::',
										_0: _elm_lang$html$Html$text('Fan Control'),
										_1: {ctor: '[]'}
									}),
								_1: {ctor: '[]'}
							}),
						_1: {
							ctor: '::',
							_0: A2(
								_debois$elm_mdl$Material_Grid$cell,
								{
									ctor: '::',
									_0: A2(_debois$elm_mdl$Material_Grid$size, _debois$elm_mdl$Material_Grid$All, 10),
									_1: {ctor: '[]'}
								},
								{
									ctor: '::',
									_0: _debois$elm_mdl$Material_Slider$view(
										{
											ctor: '::',
											_0: _debois$elm_mdl$Material_Slider$value(model.cvt.fan_voltage),
											_1: {
												ctor: '::',
												_0: _debois$elm_mdl$Material_Slider$onChange(_user$project$Main$UpdateFanVoltage),
												_1: {
													ctor: '::',
													_0: _debois$elm_mdl$Material_Slider$max(5),
													_1: {
														ctor: '::',
														_0: _debois$elm_mdl$Material_Slider$min(0),
														_1: {
															ctor: '::',
															_0: _debois$elm_mdl$Material_Slider$step(0.1),
															_1: {ctor: '[]'}
														}
													}
												}
											}
										}),
									_1: {ctor: '[]'}
								}),
							_1: {
								ctor: '::',
								_0: A2(
									_debois$elm_mdl$Material_Grid$cell,
									{
										ctor: '::',
										_0: A2(_debois$elm_mdl$Material_Grid$size, _debois$elm_mdl$Material_Grid$All, 2),
										_1: {ctor: '[]'}
									},
									{
										ctor: '::',
										_0: _elm_lang$html$Html$text(
											_elm_lang$core$Basics$toString(model.cvt.fan_voltage)),
										_1: {ctor: '[]'}
									}),
								_1: {ctor: '[]'}
							}
						}
					},
					{
						ctor: '::',
						_0: A2(
							_debois$elm_mdl$Material_Grid$cell,
							{
								ctor: '::',
								_0: A2(_debois$elm_mdl$Material_Grid$size, _debois$elm_mdl$Material_Grid$All, 3),
								_1: {ctor: '[]'}
							},
							{
								ctor: '::',
								_0: A2(
									_debois$elm_mdl$Material_List$ul,
									{ctor: '[]'},
									A2(
										_elm_lang$core$List$indexedMap,
										F2(
											function (index, temp) {
												return A2(
													_debois$elm_mdl$Material_List$li,
													{ctor: '[]'},
													{
														ctor: '::',
														_0: A2(
															_debois$elm_mdl$Material_List$content,
															{ctor: '[]'},
															{
																ctor: '::',
																_0: _elm_lang$html$Html$text(temp),
																_1: {ctor: '[]'}
															}),
														_1: {
															ctor: '::',
															_0: A2(
																_debois$elm_mdl$Material_List$content2,
																{ctor: '[]'},
																{
																	ctor: '::',
																	_0: _elm_lang$html$Html$text(
																		_user$project$Main$printableNumeric(
																			A2(_user$project$GcrdTypes$getTemperature, index, model.genData.temperatures))),
																	_1: {ctor: '[]'}
																}),
															_1: {ctor: '[]'}
														}
													});
											}),
										{
											ctor: '::',
											_0: 'PAS Channel 1',
											_1: {
												ctor: '::',
												_0: 'PAS Channel 2',
												_1: {
													ctor: '::',
													_0: 'PAS Laser Head 1',
													_1: {
														ctor: '::',
														_0: 'PAS Laser Head 2',
														_1: {
															ctor: '::',
															_0: 'Box Exit',
															_1: {
																ctor: '::',
																_0: 'CRD Heater',
																_1: {
																	ctor: '::',
																	_0: 'Box Inlet',
																	_1: {
																		ctor: '::',
																		_0: 'CRD Laser Head',
																		_1: {
																			ctor: '::',
																			_0: 'CJC 1',
																			_1: {ctor: '[]'}
																		}
																	}
																}
															}
														}
													}
												}
											}
										})),
								_1: {ctor: '[]'}
							}),
						_1: {
							ctor: '::',
							_0: A2(
								_debois$elm_mdl$Material_Grid$cell,
								{
									ctor: '::',
									_0: A2(_debois$elm_mdl$Material_Grid$size, _debois$elm_mdl$Material_Grid$All, 12),
									_1: {ctor: '[]'}
								},
								{
									ctor: '::',
									_0: A3(
										_debois$elm_mdl$Material_Options$styled,
										_elm_lang$html$Html$p,
										{
											ctor: '::',
											_0: _debois$elm_mdl$Material_Typography$headline,
											_1: {ctor: '[]'}
										},
										{
											ctor: '::',
											_0: _elm_lang$html$Html$text('Device List'),
											_1: {ctor: '[]'}
										}),
									_1: {ctor: '[]'}
								}),
							_1: {
								ctor: '::',
								_0: A2(
									_debois$elm_mdl$Material_Grid$cell,
									{
										ctor: '::',
										_0: A2(_debois$elm_mdl$Material_Grid$size, _debois$elm_mdl$Material_Grid$All, 12),
										_1: {ctor: '[]'}
									},
									{
										ctor: '::',
										_0: A2(
											_debois$elm_mdl$Material_Table$table,
											{ctor: '[]'},
											{
												ctor: '::',
												_0: A2(
													_debois$elm_mdl$Material_Table$thead,
													{ctor: '[]'},
													{
														ctor: '::',
														_0: A2(
															_debois$elm_mdl$Material_Table$tr,
															{ctor: '[]'},
															{
																ctor: '::',
																_0: A2(
																	_debois$elm_mdl$Material_Table$th,
																	{ctor: '[]'},
																	{
																		ctor: '::',
																		_0: _elm_lang$html$Html$text('Device'),
																		_1: {ctor: '[]'}
																	}),
																_1: {
																	ctor: '::',
																	_0: A2(
																		_debois$elm_mdl$Material_Table$th,
																		{ctor: '[]'},
																		{
																			ctor: '::',
																			_0: _elm_lang$html$Html$text('Type'),
																			_1: {ctor: '[]'}
																		}),
																	_1: {
																		ctor: '::',
																		_0: A2(
																			_debois$elm_mdl$Material_Table$th,
																			{ctor: '[]'},
																			{
																				ctor: '::',
																				_0: _elm_lang$html$Html$text('SN'),
																				_1: {ctor: '[]'}
																			}),
																		_1: {
																			ctor: '::',
																			_0: A2(
																				_debois$elm_mdl$Material_Table$th,
																				{ctor: '[]'},
																				{
																					ctor: '::',
																					_0: _elm_lang$html$Html$text('Address'),
																					_1: {ctor: '[]'}
																				}),
																			_1: {
																				ctor: '::',
																				_0: A2(
																					_debois$elm_mdl$Material_Table$th,
																					{ctor: '[]'},
																					{
																						ctor: '::',
																						_0: _elm_lang$html$Html$text('Model'),
																						_1: {ctor: '[]'}
																					}),
																				_1: {
																					ctor: '::',
																					_0: A2(
																						_debois$elm_mdl$Material_Table$th,
																						{ctor: '[]'},
																						{
																							ctor: '::',
																							_0: _elm_lang$html$Html$text('Active?'),
																							_1: {ctor: '[]'}
																						}),
																					_1: {ctor: '[]'}
																				}
																			}
																		}
																	}
																}
															}),
														_1: {ctor: '[]'}
													}),
												_1: {
													ctor: '::',
													_0: A2(
														_debois$elm_mdl$Material_Table$tbody,
														{ctor: '[]'},
														A2(
															_elm_lang$core$List$map,
															function (dev) {
																var device = _elm_lang$core$Tuple$second(dev);
																var c = device.active ? 'black' : 'red';
																return A2(
																	_debois$elm_mdl$Material_Table$tr,
																	{
																		ctor: '::',
																		_0: A2(_debois$elm_mdl$Material_Options$css, 'color', c),
																		_1: {ctor: '[]'}
																	},
																	{
																		ctor: '::',
																		_0: A2(
																			_debois$elm_mdl$Material_Table$td,
																			{ctor: '[]'},
																			{
																				ctor: '::',
																				_0: _elm_lang$html$Html$text(device.label),
																				_1: {ctor: '[]'}
																			}),
																		_1: {
																			ctor: '::',
																			_0: A2(
																				_debois$elm_mdl$Material_Table$td,
																				{ctor: '[]'},
																				{
																					ctor: '::',
																					_0: _elm_lang$html$Html$text(device.type_),
																					_1: {ctor: '[]'}
																				}),
																			_1: {
																				ctor: '::',
																				_0: A2(
																					_debois$elm_mdl$Material_Table$td,
																					{ctor: '[]'},
																					{
																						ctor: '::',
																						_0: _elm_lang$html$Html$text(device.sn),
																						_1: {ctor: '[]'}
																					}),
																				_1: {
																					ctor: '::',
																					_0: A2(
																						_debois$elm_mdl$Material_Table$td,
																						{ctor: '[]'},
																						{
																							ctor: '::',
																							_0: _elm_lang$html$Html$text(device.address),
																							_1: {ctor: '[]'}
																						}),
																					_1: {
																						ctor: '::',
																						_0: A2(
																							_debois$elm_mdl$Material_Table$td,
																							{ctor: '[]'},
																							{
																								ctor: '::',
																								_0: _elm_lang$html$Html$text(device.model),
																								_1: {ctor: '[]'}
																							}),
																						_1: {
																							ctor: '::',
																							_0: A2(
																								_debois$elm_mdl$Material_Table$td,
																								{ctor: '[]'},
																								{
																									ctor: '::',
																									_0: _elm_lang$html$Html$text(
																										_elm_lang$core$Basics$toString(device.active)),
																									_1: {ctor: '[]'}
																								}),
																							_1: {ctor: '[]'}
																						}
																					}
																				}
																			}
																		}
																	});
															},
															_elm_lang$core$List$concat(
																{
																	ctor: '::',
																	_0: _elm_lang$core$Dict$toList(model.alicats.cvt),
																	_1: {
																		ctor: '::',
																		_0: _elm_lang$core$Dict$toList(model.vaisalas.cvt),
																		_1: {
																			ctor: '::',
																			_0: _elm_lang$core$Dict$toList(model.ppts.cvt),
																			_1: {ctor: '[]'}
																		}
																	}
																}))),
													_1: {ctor: '[]'}
												}
											}),
										_1: {ctor: '[]'}
									}),
								_1: {
									ctor: '::',
									_0: A2(
										_debois$elm_mdl$Material_Grid$cell,
										{
											ctor: '::',
											_0: A2(_debois$elm_mdl$Material_Grid$size, _debois$elm_mdl$Material_Grid$All, 12),
											_1: {ctor: '[]'}
										},
										{
											ctor: '::',
											_0: A3(
												_debois$elm_mdl$Material_Options$styled,
												_elm_lang$html$Html$p,
												{
													ctor: '::',
													_0: _debois$elm_mdl$Material_Typography$headline,
													_1: {ctor: '[]'}
												},
												{
													ctor: '::',
													_0: _elm_lang$html$Html$text('Alicat'),
													_1: {ctor: '[]'}
												}),
											_1: {ctor: '[]'}
										}),
									_1: {
										ctor: '::',
										_0: A2(
											_debois$elm_mdl$Material_Grid$cell,
											{
												ctor: '::',
												_0: A2(_debois$elm_mdl$Material_Grid$size, _debois$elm_mdl$Material_Grid$All, 2),
												_1: {ctor: '[]'}
											},
											A2(
												_elm_lang$core$List$indexedMap,
												F2(
													function (i, dev) {
														var device = _elm_lang$core$Tuple$second(dev);
														var idx = _elm_lang$core$Tuple$first(dev);
														var m = (device.active && device.controller) ? A5(
															_debois$elm_mdl$Material_Textfield$render,
															_user$project$Main$Mdl,
															{
																ctor: '::',
																_0: 12 + i,
																_1: {ctor: '[]'}
															},
															model.mdl,
															{
																ctor: '::',
																_0: _debois$elm_mdl$Material_Textfield$floatingLabel,
																_1: {
																	ctor: '::',
																	_0: A2(_debois$elm_mdl$Material_Options$css, 'width', '125px'),
																	_1: {
																		ctor: '::',
																		_0: _debois$elm_mdl$Material_Textfield$maxlength(15),
																		_1: {
																			ctor: '::',
																			_0: _debois$elm_mdl$Material_Textfield$value(
																				A2(_elm_lang$core$Maybe$withDefault, '0', device.sp)),
																			_1: {
																				ctor: '::',
																				_0: _debois$elm_mdl$Material_Options$onInput(
																					_user$project$Main$UpdateDevSP(idx)),
																				_1: {
																					ctor: '::',
																					_0: _debois$elm_mdl$Material_Options$onBlur(
																						_user$project$Main$SendDevSP(idx)),
																					_1: {
																						ctor: '::',
																						_0: _debois$elm_mdl$Material_Textfield$label(device.label),
																						_1: {ctor: '[]'}
																					}
																				}
																			}
																		}
																	}
																}
															},
															{ctor: '[]'}) : _elm_lang$html$Html$text('');
														return m;
													}),
												_elm_lang$core$Dict$toList(model.alicats.cvt))),
										_1: {
											ctor: '::',
											_0: A2(
												_debois$elm_mdl$Material_Grid$cell,
												{
													ctor: '::',
													_0: A2(_debois$elm_mdl$Material_Grid$size, _debois$elm_mdl$Material_Grid$All, 10),
													_1: {ctor: '[]'}
												},
												{
													ctor: '::',
													_0: A2(
														_debois$elm_mdl$Material_Table$table,
														{ctor: '[]'},
														{
															ctor: '::',
															_0: A2(
																_debois$elm_mdl$Material_Table$thead,
																{ctor: '[]'},
																{
																	ctor: '::',
																	_0: A2(
																		_debois$elm_mdl$Material_Table$tr,
																		{ctor: '[]'},
																		{
																			ctor: '::',
																			_0: A2(
																				_debois$elm_mdl$Material_Table$th,
																				{ctor: '[]'},
																				{
																					ctor: '::',
																					_0: _elm_lang$html$Html$text('ID'),
																					_1: {ctor: '[]'}
																				}),
																			_1: {
																				ctor: '::',
																				_0: A2(
																					_debois$elm_mdl$Material_Table$th,
																					{ctor: '[]'},
																					{
																						ctor: '::',
																						_0: _elm_lang$html$Html$text('Pressure'),
																						_1: {ctor: '[]'}
																					}),
																				_1: {
																					ctor: '::',
																					_0: A2(
																						_debois$elm_mdl$Material_Table$th,
																						{ctor: '[]'},
																						{
																							ctor: '::',
																							_0: _elm_lang$html$Html$text('Temperature'),
																							_1: {ctor: '[]'}
																						}),
																					_1: {
																						ctor: '::',
																						_0: A2(
																							_debois$elm_mdl$Material_Table$th,
																							{ctor: '[]'},
																							{
																								ctor: '::',
																								_0: _elm_lang$html$Html$text('Flow Rate'),
																								_1: {ctor: '[]'}
																							}),
																						_1: {
																							ctor: '::',
																							_0: A2(
																								_debois$elm_mdl$Material_Table$th,
																								{ctor: '[]'},
																								{
																									ctor: '::',
																									_0: _elm_lang$html$Html$text('Setpoint'),
																									_1: {ctor: '[]'}
																								}),
																							_1: {
																								ctor: '::',
																								_0: A2(
																									_debois$elm_mdl$Material_Table$th,
																									{ctor: '[]'},
																									{
																										ctor: '::',
																										_0: _elm_lang$html$Html$text('Mass Flow Rate'),
																										_1: {ctor: '[]'}
																									}),
																								_1: {ctor: '[]'}
																							}
																						}
																					}
																				}
																			}
																		}),
																	_1: {
																		ctor: '::',
																		_0: A2(
																			_debois$elm_mdl$Material_Table$tr,
																			{ctor: '[]'},
																			{
																				ctor: '::',
																				_0: A2(
																					_debois$elm_mdl$Material_Table$th,
																					{ctor: '[]'},
																					{ctor: '[]'}),
																				_1: {
																					ctor: '::',
																					_0: A2(
																						_debois$elm_mdl$Material_Table$th,
																						{ctor: '[]'},
																						{
																							ctor: '::',
																							_0: _elm_lang$html$Html$text('mb'),
																							_1: {ctor: '[]'}
																						}),
																					_1: {
																						ctor: '::',
																						_0: A2(
																							_debois$elm_mdl$Material_Table$th,
																							{ctor: '[]'},
																							{
																								ctor: '::',
																								_0: _elm_lang$html$Html$text('degC'),
																								_1: {ctor: '[]'}
																							}),
																						_1: {
																							ctor: '::',
																							_0: A2(
																								_debois$elm_mdl$Material_Table$th,
																								{ctor: '[]'},
																								{
																									ctor: '::',
																									_0: _elm_lang$html$Html$text('lpm'),
																									_1: {ctor: '[]'}
																								}),
																							_1: {
																								ctor: '::',
																								_0: A2(
																									_debois$elm_mdl$Material_Table$th,
																									{ctor: '[]'},
																									{
																										ctor: '::',
																										_0: _elm_lang$html$Html$text('lpm'),
																										_1: {ctor: '[]'}
																									}),
																								_1: {
																									ctor: '::',
																									_0: A2(
																										_debois$elm_mdl$Material_Table$th,
																										{ctor: '[]'},
																										{
																											ctor: '::',
																											_0: _elm_lang$html$Html$text('slpm'),
																											_1: {ctor: '[]'}
																										}),
																									_1: {ctor: '[]'}
																								}
																							}
																						}
																					}
																				}
																			}),
																		_1: {ctor: '[]'}
																	}
																}),
															_1: {
																ctor: '::',
																_0: A2(
																	_debois$elm_mdl$Material_Table$tbody,
																	{ctor: '[]'},
																	A2(
																		_elm_lang$core$List$map,
																		function (dev) {
																			var dcvt = _elm_lang$core$Tuple$second(dev);
																			var id = _elm_lang$core$Tuple$first(dev);
																			var data = A2(
																				_elm_lang$core$Maybe$withDefault,
																				_user$project$Devices_Alicat$defaultData,
																				A2(_elm_lang$core$Dict$get, id, model.alicats.data));
																			return A2(
																				_debois$elm_mdl$Material_Table$tr,
																				{ctor: '[]'},
																				{
																					ctor: '::',
																					_0: A2(
																						_debois$elm_mdl$Material_Table$td,
																						{ctor: '[]'},
																						{
																							ctor: '::',
																							_0: _elm_lang$html$Html$text(dcvt.label),
																							_1: {ctor: '[]'}
																						}),
																					_1: {
																						ctor: '::',
																						_0: A2(
																							_debois$elm_mdl$Material_Table$td,
																							{ctor: '[]'},
																							{
																								ctor: '::',
																								_0: _elm_lang$html$Html$text(
																									_elm_lang$core$Basics$toString(
																										A2(_elm_lang$core$Maybe$withDefault, 0, data.pressure))),
																								_1: {ctor: '[]'}
																							}),
																						_1: {
																							ctor: '::',
																							_0: A2(
																								_debois$elm_mdl$Material_Table$td,
																								{ctor: '[]'},
																								{
																									ctor: '::',
																									_0: _elm_lang$html$Html$text(
																										_elm_lang$core$Basics$toString(
																											A2(_elm_lang$core$Maybe$withDefault, 0, data.temperature))),
																									_1: {ctor: '[]'}
																								}),
																							_1: {
																								ctor: '::',
																								_0: A2(
																									_debois$elm_mdl$Material_Table$td,
																									{ctor: '[]'},
																									{
																										ctor: '::',
																										_0: _elm_lang$html$Html$text(
																											_elm_lang$core$Basics$toString(data.output)),
																										_1: {ctor: '[]'}
																									}),
																								_1: {
																									ctor: '::',
																									_0: A2(
																										_debois$elm_mdl$Material_Table$td,
																										{ctor: '[]'},
																										{
																											ctor: '::',
																											_0: _elm_lang$html$Html$text(
																												_elm_lang$core$Basics$toString(
																													A2(_elm_lang$core$Maybe$withDefault, 0, data.setpoint))),
																											_1: {ctor: '[]'}
																										}),
																									_1: {
																										ctor: '::',
																										_0: A2(
																											_debois$elm_mdl$Material_Table$td,
																											{ctor: '[]'},
																											{
																												ctor: '::',
																												_0: _elm_lang$html$Html$text(
																													_elm_lang$core$Basics$toString(
																														A2(_elm_lang$core$Maybe$withDefault, 0, data.mass_flow))),
																												_1: {ctor: '[]'}
																											}),
																										_1: {ctor: '[]'}
																									}
																								}
																							}
																						}
																					}
																				});
																		},
																		_elm_lang$core$Dict$toList(model.alicats.cvt))),
																_1: {ctor: '[]'}
															}
														}),
													_1: {ctor: '[]'}
												}),
											_1: {
												ctor: '::',
												_0: A2(
													_debois$elm_mdl$Material_Grid$cell,
													{
														ctor: '::',
														_0: A2(_debois$elm_mdl$Material_Grid$size, _debois$elm_mdl$Material_Grid$All, 12),
														_1: {ctor: '[]'}
													},
													{
														ctor: '::',
														_0: A3(
															_debois$elm_mdl$Material_Options$styled,
															_elm_lang$html$Html$p,
															{
																ctor: '::',
																_0: _debois$elm_mdl$Material_Typography$headline,
																_1: {ctor: '[]'}
															},
															{
																ctor: '::',
																_0: _elm_lang$html$Html$text('Vaisala'),
																_1: {ctor: '[]'}
															}),
														_1: {ctor: '[]'}
													}),
												_1: {
													ctor: '::',
													_0: A2(
														_debois$elm_mdl$Material_Grid$cell,
														{
															ctor: '::',
															_0: A2(_debois$elm_mdl$Material_Grid$size, _debois$elm_mdl$Material_Grid$All, 10),
															_1: {ctor: '[]'}
														},
														{
															ctor: '::',
															_0: A2(
																_debois$elm_mdl$Material_Table$table,
																{ctor: '[]'},
																{
																	ctor: '::',
																	_0: A2(
																		_debois$elm_mdl$Material_Table$thead,
																		{ctor: '[]'},
																		{
																			ctor: '::',
																			_0: A2(
																				_debois$elm_mdl$Material_Table$tr,
																				{ctor: '[]'},
																				{
																					ctor: '::',
																					_0: A2(
																						_debois$elm_mdl$Material_Table$th,
																						{ctor: '[]'},
																						{
																							ctor: '::',
																							_0: _elm_lang$html$Html$text('ID'),
																							_1: {ctor: '[]'}
																						}),
																					_1: {
																						ctor: '::',
																						_0: A2(
																							_debois$elm_mdl$Material_Table$th,
																							{ctor: '[]'},
																							{
																								ctor: '::',
																								_0: _elm_lang$html$Html$text('Temperature'),
																								_1: {ctor: '[]'}
																							}),
																						_1: {
																							ctor: '::',
																							_0: A2(
																								_debois$elm_mdl$Material_Table$th,
																								{ctor: '[]'},
																								{
																									ctor: '::',
																									_0: _elm_lang$html$Html$text('Relative Humidity'),
																									_1: {ctor: '[]'}
																								}),
																							_1: {
																								ctor: '::',
																								_0: A2(
																									_debois$elm_mdl$Material_Table$th,
																									{ctor: '[]'},
																									{
																										ctor: '::',
																										_0: _elm_lang$html$Html$text('Dew Point'),
																										_1: {ctor: '[]'}
																									}),
																								_1: {ctor: '[]'}
																							}
																						}
																					}
																				}),
																			_1: {
																				ctor: '::',
																				_0: A2(
																					_debois$elm_mdl$Material_Table$tr,
																					{ctor: '[]'},
																					{
																						ctor: '::',
																						_0: A2(
																							_debois$elm_mdl$Material_Table$th,
																							{ctor: '[]'},
																							{ctor: '[]'}),
																						_1: {
																							ctor: '::',
																							_0: A2(
																								_debois$elm_mdl$Material_Table$th,
																								{ctor: '[]'},
																								{
																									ctor: '::',
																									_0: _elm_lang$html$Html$text('degC'),
																									_1: {ctor: '[]'}
																								}),
																							_1: {
																								ctor: '::',
																								_0: A2(
																									_debois$elm_mdl$Material_Table$th,
																									{ctor: '[]'},
																									{
																										ctor: '::',
																										_0: _elm_lang$html$Html$text('%'),
																										_1: {ctor: '[]'}
																									}),
																								_1: {
																									ctor: '::',
																									_0: A2(
																										_debois$elm_mdl$Material_Table$th,
																										{ctor: '[]'},
																										{
																											ctor: '::',
																											_0: _elm_lang$html$Html$text('degC'),
																											_1: {ctor: '[]'}
																										}),
																									_1: {ctor: '[]'}
																								}
																							}
																						}
																					}),
																				_1: {ctor: '[]'}
																			}
																		}),
																	_1: {
																		ctor: '::',
																		_0: A2(
																			_debois$elm_mdl$Material_Table$tbody,
																			{ctor: '[]'},
																			A2(
																				_elm_lang$core$List$map,
																				function (dev) {
																					var dcvt = _elm_lang$core$Tuple$second(dev);
																					var id = _elm_lang$core$Tuple$first(dev);
																					var data = A2(
																						_elm_lang$core$Maybe$withDefault,
																						A3(_user$project$Devices_Vaisala$Data, 0, 0, 0),
																						A2(_elm_lang$core$Dict$get, id, model.vaisalas.data));
																					return A2(
																						_debois$elm_mdl$Material_Table$tr,
																						{ctor: '[]'},
																						{
																							ctor: '::',
																							_0: A2(
																								_debois$elm_mdl$Material_Table$td,
																								{ctor: '[]'},
																								{
																									ctor: '::',
																									_0: _elm_lang$html$Html$text(dcvt.label),
																									_1: {ctor: '[]'}
																								}),
																							_1: {
																								ctor: '::',
																								_0: A2(
																									_debois$elm_mdl$Material_Table$td,
																									{ctor: '[]'},
																									{
																										ctor: '::',
																										_0: _elm_lang$html$Html$text(
																											_elm_lang$core$Basics$toString(data.temperature)),
																										_1: {ctor: '[]'}
																									}),
																								_1: {
																									ctor: '::',
																									_0: A2(
																										_debois$elm_mdl$Material_Table$td,
																										{ctor: '[]'},
																										{
																											ctor: '::',
																											_0: _elm_lang$html$Html$text(
																												_elm_lang$core$Basics$toString(data.relative_humidity)),
																											_1: {ctor: '[]'}
																										}),
																									_1: {
																										ctor: '::',
																										_0: A2(
																											_debois$elm_mdl$Material_Table$td,
																											{ctor: '[]'},
																											{
																												ctor: '::',
																												_0: _elm_lang$html$Html$text(
																													_elm_lang$core$Basics$toString(data.dewpoint)),
																												_1: {ctor: '[]'}
																											}),
																										_1: {ctor: '[]'}
																									}
																								}
																							}
																						});
																				},
																				_elm_lang$core$Dict$toList(model.vaisalas.cvt))),
																		_1: {ctor: '[]'}
																	}
																}),
															_1: {ctor: '[]'}
														}),
													_1: {
														ctor: '::',
														_0: A2(
															_debois$elm_mdl$Material_Grid$cell,
															{
																ctor: '::',
																_0: A2(_debois$elm_mdl$Material_Grid$size, _debois$elm_mdl$Material_Grid$All, 12),
																_1: {ctor: '[]'}
															},
															{
																ctor: '::',
																_0: A3(
																	_debois$elm_mdl$Material_Options$styled,
																	_elm_lang$html$Html$p,
																	{
																		ctor: '::',
																		_0: _debois$elm_mdl$Material_Typography$headline,
																		_1: {ctor: '[]'}
																	},
																	{
																		ctor: '::',
																		_0: _elm_lang$html$Html$text('Honeywell PPT'),
																		_1: {ctor: '[]'}
																	}),
																_1: {ctor: '[]'}
															}),
														_1: {
															ctor: '::',
															_0: A2(
																_debois$elm_mdl$Material_Grid$cell,
																{
																	ctor: '::',
																	_0: A2(_debois$elm_mdl$Material_Grid$size, _debois$elm_mdl$Material_Grid$All, 10),
																	_1: {ctor: '[]'}
																},
																{
																	ctor: '::',
																	_0: A2(
																		_debois$elm_mdl$Material_Table$table,
																		{ctor: '[]'},
																		{
																			ctor: '::',
																			_0: A2(
																				_debois$elm_mdl$Material_Table$thead,
																				{ctor: '[]'},
																				{
																					ctor: '::',
																					_0: A2(
																						_debois$elm_mdl$Material_Table$tr,
																						{ctor: '[]'},
																						{
																							ctor: '::',
																							_0: A2(
																								_debois$elm_mdl$Material_Table$th,
																								{ctor: '[]'},
																								{
																									ctor: '::',
																									_0: _elm_lang$html$Html$text('ID'),
																									_1: {ctor: '[]'}
																								}),
																							_1: {
																								ctor: '::',
																								_0: A2(
																									_debois$elm_mdl$Material_Table$th,
																									{ctor: '[]'},
																									{
																										ctor: '::',
																										_0: _elm_lang$html$Html$text('Pressure'),
																										_1: {ctor: '[]'}
																									}),
																								_1: {
																									ctor: '::',
																									_0: A2(
																										_debois$elm_mdl$Material_Table$th,
																										{ctor: '[]'},
																										{
																											ctor: '::',
																											_0: _elm_lang$html$Html$text('Temperature'),
																											_1: {ctor: '[]'}
																										}),
																									_1: {ctor: '[]'}
																								}
																							}
																						}),
																					_1: {
																						ctor: '::',
																						_0: A2(
																							_debois$elm_mdl$Material_Table$tr,
																							{ctor: '[]'},
																							{
																								ctor: '::',
																								_0: A2(
																									_debois$elm_mdl$Material_Table$th,
																									{ctor: '[]'},
																									{ctor: '[]'}),
																								_1: {
																									ctor: '::',
																									_0: A2(
																										_debois$elm_mdl$Material_Table$th,
																										{ctor: '[]'},
																										{
																											ctor: '::',
																											_0: _elm_lang$html$Html$text('mb'),
																											_1: {ctor: '[]'}
																										}),
																									_1: {
																										ctor: '::',
																										_0: A2(
																											_debois$elm_mdl$Material_Table$th,
																											{ctor: '[]'},
																											{
																												ctor: '::',
																												_0: _elm_lang$html$Html$text('degC'),
																												_1: {ctor: '[]'}
																											}),
																										_1: {ctor: '[]'}
																									}
																								}
																							}),
																						_1: {ctor: '[]'}
																					}
																				}),
																			_1: {
																				ctor: '::',
																				_0: A2(
																					_debois$elm_mdl$Material_Table$tbody,
																					{ctor: '[]'},
																					A2(
																						_elm_lang$core$List$map,
																						function (dev) {
																							var dcvt = _elm_lang$core$Tuple$second(dev);
																							var id = _elm_lang$core$Tuple$first(dev);
																							var data = A2(
																								_elm_lang$core$Maybe$withDefault,
																								A2(_user$project$Devices_Ppt$Data, 0, 0),
																								A2(_elm_lang$core$Dict$get, id, model.ppts.data));
																							return A2(
																								_debois$elm_mdl$Material_Table$tr,
																								{ctor: '[]'},
																								{
																									ctor: '::',
																									_0: A2(
																										_debois$elm_mdl$Material_Table$td,
																										{ctor: '[]'},
																										{
																											ctor: '::',
																											_0: _elm_lang$html$Html$text(dcvt.label),
																											_1: {ctor: '[]'}
																										}),
																									_1: {
																										ctor: '::',
																										_0: A2(
																											_debois$elm_mdl$Material_Table$td,
																											{ctor: '[]'},
																											{
																												ctor: '::',
																												_0: _elm_lang$html$Html$text(
																													_elm_lang$core$Basics$toString(data.pressure)),
																												_1: {ctor: '[]'}
																											}),
																										_1: {
																											ctor: '::',
																											_0: A2(
																												_debois$elm_mdl$Material_Table$td,
																												{ctor: '[]'},
																												{
																													ctor: '::',
																													_0: _elm_lang$html$Html$text(
																														_elm_lang$core$Basics$toString(data.temperature)),
																													_1: {ctor: '[]'}
																												}),
																											_1: {ctor: '[]'}
																										}
																									}
																								});
																						},
																						_elm_lang$core$Dict$toList(model.ppts.cvt))),
																				_1: {ctor: '[]'}
																			}
																		}),
																	_1: {ctor: '[]'}
																}),
															_1: {ctor: '[]'}
														}
													}
												}
											}
										}
									}
								}
							}
						}
					}))));
};
var _user$project$Main$textfield = F6(
	function (num, model, label, value, input_msg, blur_msg) {
		return A5(
			_debois$elm_mdl$Material_Textfield$render,
			_user$project$Main$Mdl,
			{
				ctor: '::',
				_0: num,
				_1: {ctor: '[]'}
			},
			model.mdl,
			A2(
				_elm_lang$core$Basics_ops['++'],
				{
					ctor: '::',
					_0: _debois$elm_mdl$Material_Textfield$floatingLabel,
					_1: {
						ctor: '::',
						_0: A2(_debois$elm_mdl$Material_Options$css, 'width', '125px'),
						_1: {
							ctor: '::',
							_0: _debois$elm_mdl$Material_Textfield$maxlength(15),
							_1: {
								ctor: '::',
								_0: _debois$elm_mdl$Material_Textfield$value(value),
								_1: {
									ctor: '::',
									_0: _debois$elm_mdl$Material_Options$onInput(input_msg),
									_1: {ctor: '[]'}
								}
							}
						}
					}
				},
				A2(
					_elm_lang$core$Basics_ops['++'],
					A2(
						_elm_lang$core$Maybe$withDefault,
						{ctor: '[]'},
						A2(
							_elm_lang$core$Maybe$map,
							function (lab) {
								return {
									ctor: '::',
									_0: _debois$elm_mdl$Material_Textfield$label(lab),
									_1: {ctor: '[]'}
								};
							},
							label)),
					A2(
						_elm_lang$core$Maybe$withDefault,
						{ctor: '[]'},
						A2(
							_elm_lang$core$Maybe$map,
							function (msg) {
								return {
									ctor: '::',
									_0: _debois$elm_mdl$Material_Options$onBlur(msg),
									_1: {ctor: '[]'}
								};
							},
							blur_msg)))),
			{ctor: '[]'});
	});
var _user$project$Main$viewConfig = function (model) {
	return A2(
		_debois$elm_mdl$Material_Grid$grid,
		{
			ctor: '::',
			_0: A2(_debois$elm_mdl$Material_Options$css, 'padding-bottom', '50px'),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: A2(
				_debois$elm_mdl$Material_Grid$cell,
				{
					ctor: '::',
					_0: A2(_debois$elm_mdl$Material_Grid$size, _debois$elm_mdl$Material_Grid$Desktop, 3),
					_1: {
						ctor: '::',
						_0: A2(_debois$elm_mdl$Material_Grid$size, _debois$elm_mdl$Material_Grid$Tablet, 5),
						_1: {
							ctor: '::',
							_0: A2(_debois$elm_mdl$Material_Grid$size, _debois$elm_mdl$Material_Grid$Phone, 3),
							_1: {ctor: '[]'}
						}
					}
				},
				{
					ctor: '::',
					_0: A6(
						_user$project$Main$textfield,
						0,
						model,
						_elm_lang$core$Maybe$Just('IP Address'),
						model.network.ip,
						function (_p23) {
							return _user$project$Main$Network(
								_user$project$Network$UpdateIP(_p23));
						},
						_elm_lang$core$Maybe$Nothing),
					_1: {
						ctor: '::',
						_0: A6(
							_user$project$Main$textfield,
							1,
							model,
							_elm_lang$core$Maybe$Just('Port'),
							model.network.port_,
							function (_p24) {
								return _user$project$Main$Network(
									_user$project$Network$UpdatePort(_p24));
							},
							_elm_lang$core$Maybe$Nothing),
						_1: {ctor: '[]'}
					}
				}),
			_1: {ctor: '[]'}
		});
};
var _user$project$Main$floatTextfield = F6(
	function (num, model, label, value, input_msg, blur_msg) {
		return A5(
			_debois$elm_mdl$Material_Textfield$render,
			_user$project$Main$Mdl,
			{
				ctor: '::',
				_0: num,
				_1: {ctor: '[]'}
			},
			model.mdl,
			A2(
				_elm_lang$core$Basics_ops['++'],
				{
					ctor: '::',
					_0: _debois$elm_mdl$Material_Textfield$floatingLabel,
					_1: {
						ctor: '::',
						_0: A2(_debois$elm_mdl$Material_Options$css, 'width', '125px'),
						_1: {
							ctor: '::',
							_0: _debois$elm_mdl$Material_Textfield$maxlength(15),
							_1: {
								ctor: '::',
								_0: _debois$elm_mdl$Material_Textfield$value(
									_elm_lang$core$Basics$toString(value)),
								_1: {
									ctor: '::',
									_0: _debois$elm_mdl$Material_Options$onInput(input_msg),
									_1: {ctor: '[]'}
								}
							}
						}
					}
				},
				A2(
					_elm_lang$core$Basics_ops['++'],
					A2(
						_elm_lang$core$Maybe$withDefault,
						{ctor: '[]'},
						A2(
							_elm_lang$core$Maybe$map,
							function (lab) {
								return {
									ctor: '::',
									_0: _debois$elm_mdl$Material_Textfield$label(lab),
									_1: {ctor: '[]'}
								};
							},
							label)),
					A2(
						_elm_lang$core$Maybe$withDefault,
						{ctor: '[]'},
						A2(
							_elm_lang$core$Maybe$map,
							function (msg) {
								return {
									ctor: '::',
									_0: _debois$elm_mdl$Material_Options$onBlur(msg),
									_1: {ctor: '[]'}
								};
							},
							blur_msg)))),
			{ctor: '[]'});
	});
var _user$project$Main$button = F4(
	function (num, btn_txt, model, msg) {
		return A5(
			_debois$elm_mdl$Material_Button$render,
			_user$project$Main$Mdl,
			{
				ctor: '::',
				_0: num,
				_1: {ctor: '[]'}
			},
			model.mdl,
			{
				ctor: '::',
				_0: _debois$elm_mdl$Material_Button$raised,
				_1: {
					ctor: '::',
					_0: _debois$elm_mdl$Material_Button$ripple,
					_1: {
						ctor: '::',
						_0: _debois$elm_mdl$Material_Button$primary,
						_1: {
							ctor: '::',
							_0: _debois$elm_mdl$Material_Options$onClick(msg),
							_1: {
								ctor: '::',
								_0: A2(_debois$elm_mdl$Material_Options$css, 'margin-top', '10px'),
								_1: {
									ctor: '::',
									_0: A2(_debois$elm_mdl$Material_Options$css, 'width', '125px'),
									_1: {ctor: '[]'}
								}
							}
						}
					}
				}
			},
			{
				ctor: '::',
				_0: _elm_lang$html$Html$text(btn_txt),
				_1: {ctor: '[]'}
			});
	});
var _user$project$Main$viewDrawer = function (model) {
	return {
		ctor: '::',
		_0: A5(
			_debois$elm_mdl$Material_Toggles$switch,
			_user$project$Main$Mdl,
			{
				ctor: '::',
				_0: 10,
				_1: {ctor: '[]'}
			},
			model.mdl,
			{
				ctor: '::',
				_0: _debois$elm_mdl$Material_Toggles$ripple,
				_1: {
					ctor: '::',
					_0: _debois$elm_mdl$Material_Toggles$value(model.cvt.save),
					_1: {
						ctor: '::',
						_0: _debois$elm_mdl$Material_Options$onToggle(_user$project$Main$SaveData),
						_1: {ctor: '[]'}
					}
				}
			},
			{
				ctor: '::',
				_0: _elm_lang$html$Html$text('Save'),
				_1: {ctor: '[]'}
			}),
		_1: {
			ctor: '::',
			_0: A4(_user$project$Main$button, 1, 'Stop Server', model, _user$project$Main$StopServer),
			_1: {
				ctor: '::',
				_0: A4(_user$project$Main$button, 9, 'Sync Time', model, _user$project$Main$SyncTime),
				_1: {
					ctor: '::',
					_0: A5(
						_debois$elm_mdl$Material_Toggles$radio,
						_user$project$Main$Mdl,
						{
							ctor: '::',
							_0: 2,
							_1: {ctor: '[]'}
						},
						model.mdl,
						{
							ctor: '::',
							_0: _debois$elm_mdl$Material_Toggles$value(false),
							_1: {
								ctor: '::',
								_0: _debois$elm_mdl$Material_Toggles$group('FilterPath'),
								_1: {
									ctor: '::',
									_0: _debois$elm_mdl$Material_Toggles$ripple,
									_1: {
										ctor: '::',
										_0: _debois$elm_mdl$Material_Toggles$value(
											_elm_lang$core$Native_Utils.eq(model.cvt.filter.pos, 0)),
										_1: {
											ctor: '::',
											_0: _debois$elm_mdl$Material_Options$onToggle(_user$project$Main$ToggleFilter),
											_1: {
												ctor: '::',
												_0: A2(_debois$elm_mdl$Material_Options$css, 'margin-top', '10px'),
												_1: {ctor: '[]'}
											}
										}
									}
								}
							}
						},
						{
							ctor: '::',
							_0: _elm_lang$html$Html$text('Channel 1'),
							_1: {ctor: '[]'}
						}),
					_1: {
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$div,
							{ctor: '[]'},
							{
								ctor: '::',
								_0: A5(
									_debois$elm_mdl$Material_Toggles$radio,
									_user$project$Main$Mdl,
									{
										ctor: '::',
										_0: 3,
										_1: {ctor: '[]'}
									},
									model.mdl,
									{
										ctor: '::',
										_0: _debois$elm_mdl$Material_Toggles$value(true),
										_1: {
											ctor: '::',
											_0: _debois$elm_mdl$Material_Toggles$group('FilterPath'),
											_1: {
												ctor: '::',
												_0: _debois$elm_mdl$Material_Toggles$ripple,
												_1: {
													ctor: '::',
													_0: _debois$elm_mdl$Material_Toggles$value(
														_elm_lang$core$Native_Utils.eq(model.cvt.filter.pos, 1)),
													_1: {
														ctor: '::',
														_0: _debois$elm_mdl$Material_Options$onToggle(_user$project$Main$ToggleFilter),
														_1: {
															ctor: '::',
															_0: A2(_debois$elm_mdl$Material_Options$css, 'margin-top', '10px'),
															_1: {ctor: '[]'}
														}
													}
												}
											}
										}
									},
									{
										ctor: '::',
										_0: _elm_lang$html$Html$text('Channel 2'),
										_1: {ctor: '[]'}
									}),
								_1: {ctor: '[]'}
							}),
						_1: {
							ctor: '::',
							_0: A5(
								_debois$elm_mdl$Material_Toggles$switch,
								_user$project$Main$Mdl,
								{
									ctor: '::',
									_0: 7,
									_1: {ctor: '[]'}
								},
								model.mdl,
								{
									ctor: '::',
									_0: _debois$elm_mdl$Material_Toggles$ripple,
									_1: {
										ctor: '::',
										_0: _debois$elm_mdl$Material_Toggles$value(model.cvt.cal_state.o3_add),
										_1: {
											ctor: '::',
											_0: _debois$elm_mdl$Material_Options$onToggle(_user$project$Main$ToggleO3),
											_1: {
												ctor: '::',
												_0: A2(_debois$elm_mdl$Material_Options$css, 'margin-top', '10px'),
												_1: {ctor: '[]'}
											}
										}
									}
								},
								{
									ctor: '::',
									_0: _elm_lang$html$Html$text('O3 Addition'),
									_1: {ctor: '[]'}
								}),
							_1: {
								ctor: '::',
								_0: A5(
									_debois$elm_mdl$Material_Toggles$switch,
									_user$project$Main$Mdl,
									{
										ctor: '::',
										_0: 4,
										_1: {ctor: '[]'}
									},
									model.mdl,
									{
										ctor: '::',
										_0: _debois$elm_mdl$Material_Toggles$ripple,
										_1: {
											ctor: '::',
											_0: _debois$elm_mdl$Material_Toggles$value(model.cvt.cal_state.o2_add),
											_1: {
												ctor: '::',
												_0: _debois$elm_mdl$Material_Options$onToggle(_user$project$Main$ToggleO2),
												_1: {
													ctor: '::',
													_0: A2(_debois$elm_mdl$Material_Options$css, 'margin-top', '10px'),
													_1: {ctor: '[]'}
												}
											}
										}
									},
									{
										ctor: '::',
										_0: _elm_lang$html$Html$text('O2 Addition'),
										_1: {ctor: '[]'}
									}),
								_1: {
									ctor: '::',
									_0: A5(
										_debois$elm_mdl$Material_Toggles$switch,
										_user$project$Main$Mdl,
										{
											ctor: '::',
											_0: 5,
											_1: {ctor: '[]'}
										},
										model.mdl,
										{
											ctor: '::',
											_0: _debois$elm_mdl$Material_Toggles$ripple,
											_1: {
												ctor: '::',
												_0: _debois$elm_mdl$Material_Toggles$value(model.cvt.pump),
												_1: {
													ctor: '::',
													_0: _debois$elm_mdl$Material_Options$onToggle(_user$project$Main$TogglePump),
													_1: {
														ctor: '::',
														_0: A2(_debois$elm_mdl$Material_Options$css, 'margin-top', '10px'),
														_1: {ctor: '[]'}
													}
												}
											}
										},
										{
											ctor: '::',
											_0: _elm_lang$html$Html$text('Pump'),
											_1: {ctor: '[]'}
										}),
									_1: {
										ctor: '::',
										_0: A5(
											_debois$elm_mdl$Material_Toggles$switch,
											_user$project$Main$Mdl,
											{
												ctor: '::',
												_0: 6,
												_1: {ctor: '[]'}
											},
											model.mdl,
											{
												ctor: '::',
												_0: _debois$elm_mdl$Material_Toggles$ripple,
												_1: {
													ctor: '::',
													_0: _debois$elm_mdl$Material_Toggles$value(model.cvt.cal_state.uv_lamp),
													_1: {
														ctor: '::',
														_0: _debois$elm_mdl$Material_Options$onToggle(_user$project$Main$ToggleUVLamp),
														_1: {
															ctor: '::',
															_0: A2(_debois$elm_mdl$Material_Options$css, 'margin-top', '10px'),
															_1: {ctor: '[]'}
														}
													}
												}
											},
											{
												ctor: '::',
												_0: _elm_lang$html$Html$text('UV Lamp'),
												_1: {ctor: '[]'}
											}),
										_1: {
											ctor: '::',
											_0: A5(
												_debois$elm_mdl$Material_Toggles$switch,
												_user$project$Main$Mdl,
												{
													ctor: '::',
													_0: 8,
													_1: {ctor: '[]'}
												},
												model.mdl,
												{
													ctor: '::',
													_0: _debois$elm_mdl$Material_Toggles$ripple,
													_1: {
														ctor: '::',
														_0: _debois$elm_mdl$Material_Toggles$value(model.cvt.fan),
														_1: {
															ctor: '::',
															_0: _debois$elm_mdl$Material_Options$onToggle(_user$project$Main$ToggleFan),
															_1: {
																ctor: '::',
																_0: A2(_debois$elm_mdl$Material_Options$css, 'margin-top', '10px'),
																_1: {ctor: '[]'}
															}
														}
													}
												},
												{
													ctor: '::',
													_0: _elm_lang$html$Html$text('Fan'),
													_1: {ctor: '[]'}
												}),
											_1: {ctor: '[]'}
										}
									}
								}
							}
						}
					}
				}
			}
		}
	};
};
var _user$project$Main$viewCal = function (model) {
	return A2(
		_debois$elm_mdl$Material_Grid$grid,
		{ctor: '[]'},
		{
			ctor: '::',
			_0: A2(
				_debois$elm_mdl$Material_Grid$cell,
				{
					ctor: '::',
					_0: A2(_debois$elm_mdl$Material_Grid$size, _debois$elm_mdl$Material_Grid$All, 12),
					_1: {ctor: '[]'}
				},
				{
					ctor: '::',
					_0: A5(
						_debois$elm_mdl$Material_Toggles$switch,
						_user$project$Main$Mdl,
						{
							ctor: '::',
							_0: 11,
							_1: {ctor: '[]'}
						},
						model.mdl,
						{
							ctor: '::',
							_0: _debois$elm_mdl$Material_Toggles$ripple,
							_1: {
								ctor: '::',
								_0: _debois$elm_mdl$Material_Toggles$value(
									_elm_lang$core$Native_Utils.eq(model.cvt.sequence_state, 'Run')),
								_1: {
									ctor: '::',
									_0: _debois$elm_mdl$Material_Options$onToggle(_user$project$Main$SequenceState),
									_1: {ctor: '[]'}
								}
							}
						},
						{
							ctor: '::',
							_0: _elm_lang$html$Html$text('Run Sequence'),
							_1: {ctor: '[]'}
						}),
					_1: {
						ctor: '::',
						_0: A4(_user$project$Main$button, 106, 'Reset Sequence', model, _user$project$Main$ResetSequence),
						_1: {ctor: '[]'}
					}
				}),
			_1: {
				ctor: '::',
				_0: A2(
					_debois$elm_mdl$Material_Grid$cell,
					{
						ctor: '::',
						_0: A2(_debois$elm_mdl$Material_Grid$size, _debois$elm_mdl$Material_Grid$All, 3),
						_1: {ctor: '[]'}
					},
					{
						ctor: '::',
						_0: A2(
							_debois$elm_mdl$Material_List$ul,
							{ctor: '[]'},
							{
								ctor: '::',
								_0: A2(
									_debois$elm_mdl$Material_List$li,
									{
										ctor: '::',
										_0: _debois$elm_mdl$Material_List$withBody,
										_1: {ctor: '[]'}
									},
									{
										ctor: '::',
										_0: A2(
											_debois$elm_mdl$Material_List$content,
											{ctor: '[]'},
											{
												ctor: '::',
												_0: _elm_lang$html$Html$text('Speaker'),
												_1: {
													ctor: '::',
													_0: A2(
														_debois$elm_mdl$Material_List$body,
														{ctor: '[]'},
														{
															ctor: '::',
															_0: _elm_lang$html$Html$text('Use this to toggle the speaker.  '),
															_1: {ctor: '[]'}
														}),
													_1: {ctor: '[]'}
												}
											}),
										_1: {ctor: '[]'}
									}),
								_1: {
									ctor: '::',
									_0: A2(
										_debois$elm_mdl$Material_List$li,
										{
											ctor: '::',
											_0: _debois$elm_mdl$Material_List$withBody,
											_1: {ctor: '[]'}
										},
										{
											ctor: '::',
											_0: A2(
												_debois$elm_mdl$Material_List$content,
												{ctor: '[]'},
												{
													ctor: '::',
													_0: _elm_lang$html$Html$text('Flow Path'),
													_1: {
														ctor: '::',
														_0: A2(
															_debois$elm_mdl$Material_List$body,
															{ctor: '[]'},
															{
																ctor: '::',
																_0: _elm_lang$html$Html$text('Use this to toggle the flow path. '),
																_1: {ctor: '[]'}
															}),
														_1: {ctor: '[]'}
													}
												}),
											_1: {ctor: '[]'}
										}),
									_1: {
										ctor: '::',
										_0: A2(
											_debois$elm_mdl$Material_List$li,
											{
												ctor: '::',
												_0: _debois$elm_mdl$Material_List$withBody,
												_1: {ctor: '[]'}
											},
											{
												ctor: '::',
												_0: A2(
													_debois$elm_mdl$Material_List$content,
													{ctor: '[]'},
													{
														ctor: '::',
														_0: _elm_lang$html$Html$text('UV Lamp'),
														_1: {
															ctor: '::',
															_0: A2(
																_debois$elm_mdl$Material_List$body,
																{ctor: '[]'},
																{
																	ctor: '::',
																	_0: _elm_lang$html$Html$text('Toggle power to the UV lamp.'),
																	_1: {ctor: '[]'}
																}),
															_1: {ctor: '[]'}
														}
													}),
												_1: {ctor: '[]'}
											}),
										_1: {
											ctor: '::',
											_0: A2(
												_debois$elm_mdl$Material_List$li,
												{
													ctor: '::',
													_0: _debois$elm_mdl$Material_List$withBody,
													_1: {ctor: '[]'}
												},
												{
													ctor: '::',
													_0: A2(
														_debois$elm_mdl$Material_List$content,
														{ctor: '[]'},
														{
															ctor: '::',
															_0: _elm_lang$html$Html$text('O2 Valve'),
															_1: {
																ctor: '::',
																_0: A2(
																	_debois$elm_mdl$Material_List$body,
																	{ctor: '[]'},
																	{
																		ctor: '::',
																		_0: _elm_lang$html$Html$text('Toggle O2 valve.'),
																		_1: {ctor: '[]'}
																	}),
																_1: {ctor: '[]'}
															}
														}),
													_1: {ctor: '[]'}
												}),
											_1: {
												ctor: '::',
												_0: A2(
													_debois$elm_mdl$Material_List$li,
													{
														ctor: '::',
														_0: _debois$elm_mdl$Material_List$withBody,
														_1: {ctor: '[]'}
													},
													{
														ctor: '::',
														_0: A2(
															_debois$elm_mdl$Material_List$content,
															{ctor: '[]'},
															{
																ctor: '::',
																_0: _elm_lang$html$Html$text('O3 Valve'),
																_1: {
																	ctor: '::',
																	_0: A2(
																		_debois$elm_mdl$Material_List$body,
																		{ctor: '[]'},
																		{
																			ctor: '::',
																			_0: _elm_lang$html$Html$text('Toggle O3 valve.'),
																			_1: {ctor: '[]'}
																		}),
																	_1: {ctor: '[]'}
																}
															}),
														_1: {ctor: '[]'}
													}),
												_1: {ctor: '[]'}
											}
										}
									}
								}
							}),
						_1: {ctor: '[]'}
					}),
				_1: {
					ctor: '::',
					_0: A2(
						_debois$elm_mdl$Material_Grid$cell,
						{
							ctor: '::',
							_0: A2(_debois$elm_mdl$Material_Grid$size, _debois$elm_mdl$Material_Grid$All, 3),
							_1: {ctor: '[]'}
						},
						{
							ctor: '::',
							_0: A2(
								_debois$elm_mdl$Material_List$ul,
								{ctor: '[]'},
								{
									ctor: '::',
									_0: A2(
										_debois$elm_mdl$Material_List$li,
										{ctor: '[]'},
										{
											ctor: '::',
											_0: A2(
												_debois$elm_mdl$Material_List$content,
												{ctor: '[]'},
												{
													ctor: '::',
													_0: _elm_lang$html$Html$text('Speaker'),
													_1: {ctor: '[]'}
												}),
											_1: {
												ctor: '::',
												_0: A2(
													_debois$elm_mdl$Material_List$content2,
													{ctor: '[]'},
													{
														ctor: '::',
														_0: A5(
															_debois$elm_mdl$Material_Toggles$checkbox,
															_user$project$Main$Mdl,
															{
																ctor: '::',
																_0: 4,
																_1: {ctor: '[]'}
															},
															model.mdl,
															{
																ctor: '::',
																_0: _debois$elm_mdl$Material_Toggles$value(true),
																_1: {ctor: '[]'}
															},
															{ctor: '[]'}),
														_1: {ctor: '[]'}
													}),
												_1: {ctor: '[]'}
											}
										}),
									_1: {
										ctor: '::',
										_0: A2(
											_debois$elm_mdl$Material_List$li,
											{ctor: '[]'},
											{
												ctor: '::',
												_0: A2(
													_debois$elm_mdl$Material_List$content,
													{ctor: '[]'},
													{
														ctor: '::',
														_0: _elm_lang$html$Html$text('Radio button!'),
														_1: {ctor: '[]'}
													}),
												_1: {
													ctor: '::',
													_0: A2(
														_debois$elm_mdl$Material_List$content2,
														{ctor: '[]'},
														{
															ctor: '::',
															_0: A2(
																_debois$elm_mdl$Material_Options$span,
																{
																	ctor: '::',
																	_0: _debois$elm_mdl$Material_List$action2,
																	_1: {ctor: '[]'}
																},
																{
																	ctor: '::',
																	_0: A5(
																		_debois$elm_mdl$Material_Toggles$radio,
																		_user$project$Main$Mdl,
																		{
																			ctor: '::',
																			_0: 5,
																			_1: {ctor: '[]'}
																		},
																		model.mdl,
																		{
																			ctor: '::',
																			_0: _debois$elm_mdl$Material_Toggles$value(true),
																			_1: {
																				ctor: '::',
																				_0: A2(_debois$elm_mdl$Material_Options$css, 'display', 'inline'),
																				_1: {ctor: '[]'}
																			}
																		},
																		{ctor: '[]'}),
																	_1: {ctor: '[]'}
																}),
															_1: {ctor: '[]'}
														}),
													_1: {ctor: '[]'}
												}
											}),
										_1: {
											ctor: '::',
											_0: A2(
												_debois$elm_mdl$Material_List$li,
												{ctor: '[]'},
												{
													ctor: '::',
													_0: A2(
														_debois$elm_mdl$Material_List$content,
														{ctor: '[]'},
														{
															ctor: '::',
															_0: _elm_lang$html$Html$text('Include switch?'),
															_1: {ctor: '[]'}
														}),
													_1: {
														ctor: '::',
														_0: A2(
															_debois$elm_mdl$Material_List$content2,
															{ctor: '[]'},
															{
																ctor: '::',
																_0: A5(
																	_debois$elm_mdl$Material_Toggles$switch,
																	_user$project$Main$Mdl,
																	{
																		ctor: '::',
																		_0: 6,
																		_1: {ctor: '[]'}
																	},
																	model.mdl,
																	{
																		ctor: '::',
																		_0: _debois$elm_mdl$Material_Toggles$value(true),
																		_1: {ctor: '[]'}
																	},
																	{ctor: '[]'}),
																_1: {ctor: '[]'}
															}),
														_1: {ctor: '[]'}
													}
												}),
											_1: {ctor: '[]'}
										}
									}
								}),
							_1: {ctor: '[]'}
						}),
					_1: {ctor: '[]'}
				}
			}
		});
};
var _user$project$Main$viewStatus = function (model) {
	return A2(
		_debois$elm_mdl$Material_Grid$grid,
		{ctor: '[]'},
		{
			ctor: '::',
			_0: A2(
				_debois$elm_mdl$Material_Grid$cell,
				{
					ctor: '::',
					_0: A2(_debois$elm_mdl$Material_Grid$size, _debois$elm_mdl$Material_Grid$All, 12),
					_1: {ctor: '[]'}
				},
				{
					ctor: '::',
					_0: A3(
						_debois$elm_mdl$Material_Options$styled,
						_elm_lang$html$Html$p,
						{
							ctor: '::',
							_0: _debois$elm_mdl$Material_Typography$title,
							_1: {ctor: '[]'}
						},
						{
							ctor: '::',
							_0: _elm_lang$html$Html$text('System Messages'),
							_1: {ctor: '[]'}
						}),
					_1: {
						ctor: '::',
						_0: A2(
							_debois$elm_mdl$Material_Options$div,
							{
								ctor: '::',
								_0: A2(_debois$elm_mdl$Material_Options$css, 'max-height', '250px'),
								_1: {
									ctor: '::',
									_0: A2(_debois$elm_mdl$Material_Options$css, 'overflow', 'scroll'),
									_1: {
										ctor: '::',
										_0: A2(_debois$elm_mdl$Material_Options$css, 'border', '1px solid grey'),
										_1: {
											ctor: '::',
											_0: A2(_debois$elm_mdl$Material_Options$css, 'border-radius', '4px'),
											_1: {ctor: '[]'}
										}
									}
								}
							},
							A2(
								_elm_lang$core$List$map,
								function (msg) {
									var msg_color = A2(_elm_lang$core$String$contains, '[ERROR]', msg) ? 'red' : (A2(_elm_lang$core$String$contains, '[WARNING]', msg) ? 'yellow' : 'black');
									return A3(
										_debois$elm_mdl$Material_Options$styled,
										_elm_lang$html$Html$p,
										{
											ctor: '::',
											_0: _debois$elm_mdl$Material_Typography$body1,
											_1: {
												ctor: '::',
												_0: A2(_debois$elm_mdl$Material_Options$css, 'color', msg_color),
												_1: {
													ctor: '::',
													_0: A2(_debois$elm_mdl$Material_Options$css, 'width', '100%'),
													_1: {
														ctor: '::',
														_0: A2(_debois$elm_mdl$Material_Options$css, 'padding', '0'),
														_1: {
															ctor: '::',
															_0: A2(_debois$elm_mdl$Material_Options$css, 'margin', '0'),
															_1: {ctor: '[]'}
														}
													}
												}
											}
										},
										{
											ctor: '::',
											_0: _elm_lang$html$Html$text(msg),
											_1: {ctor: '[]'}
										});
								},
								model.currentMsgList)),
						_1: {
							ctor: '::',
							_0: A4(_user$project$Main$button, 10, 'Clear', model, _user$project$Main$ClearMessages),
							_1: {ctor: '[]'}
						}
					}
				}),
			_1: {ctor: '[]'}
		});
};
var _user$project$Main$intTextfield = F6(
	function (num, model, label, value, input_msg, blur_msg) {
		return A5(
			_debois$elm_mdl$Material_Textfield$render,
			_user$project$Main$Mdl,
			{
				ctor: '::',
				_0: num,
				_1: {ctor: '[]'}
			},
			model.mdl,
			A2(
				_elm_lang$core$Basics_ops['++'],
				{
					ctor: '::',
					_0: _debois$elm_mdl$Material_Textfield$floatingLabel,
					_1: {
						ctor: '::',
						_0: A2(_debois$elm_mdl$Material_Options$css, 'width', '125px'),
						_1: {
							ctor: '::',
							_0: _debois$elm_mdl$Material_Textfield$maxlength(15),
							_1: {
								ctor: '::',
								_0: _debois$elm_mdl$Material_Textfield$value(
									_elm_lang$core$Basics$toString(value)),
								_1: {
									ctor: '::',
									_0: _debois$elm_mdl$Material_Options$onInput(input_msg),
									_1: {ctor: '[]'}
								}
							}
						}
					}
				},
				A2(
					_elm_lang$core$Basics_ops['++'],
					A2(
						_elm_lang$core$Maybe$withDefault,
						{ctor: '[]'},
						A2(
							_elm_lang$core$Maybe$map,
							function (lab) {
								return {
									ctor: '::',
									_0: _debois$elm_mdl$Material_Textfield$label(lab),
									_1: {ctor: '[]'}
								};
							},
							label)),
					A2(
						_elm_lang$core$Maybe$withDefault,
						{ctor: '[]'},
						A2(
							_elm_lang$core$Maybe$map,
							function (msg) {
								return {
									ctor: '::',
									_0: _debois$elm_mdl$Material_Options$onBlur(msg),
									_1: {ctor: '[]'}
								};
							},
							blur_msg)))),
			{ctor: '[]'});
	});
var _user$project$Main$txtfield = F6(
	function (num, model, label, value, input_msg, blur_msg) {
		return A5(
			_debois$elm_mdl$Material_Textfield$render,
			_user$project$Main$Mdl,
			{
				ctor: '::',
				_0: num,
				_1: {ctor: '[]'}
			},
			model.mdl,
			A2(
				_elm_lang$core$Basics_ops['++'],
				{
					ctor: '::',
					_0: _debois$elm_mdl$Material_Textfield$floatingLabel,
					_1: {
						ctor: '::',
						_0: A2(_debois$elm_mdl$Material_Options$css, 'width', '125px'),
						_1: {
							ctor: '::',
							_0: _debois$elm_mdl$Material_Textfield$maxlength(15),
							_1: {
								ctor: '::',
								_0: _debois$elm_mdl$Material_Textfield$value(value),
								_1: {
									ctor: '::',
									_0: _debois$elm_mdl$Material_Options$onInput(input_msg),
									_1: {ctor: '[]'}
								}
							}
						}
					}
				},
				A2(
					_elm_lang$core$Basics_ops['++'],
					A2(
						_elm_lang$core$Maybe$withDefault,
						{ctor: '[]'},
						A2(
							_elm_lang$core$Maybe$map,
							function (lab) {
								return {
									ctor: '::',
									_0: _debois$elm_mdl$Material_Textfield$label(lab),
									_1: {ctor: '[]'}
								};
							},
							label)),
					A2(
						_elm_lang$core$Maybe$withDefault,
						{ctor: '[]'},
						A2(
							_elm_lang$core$Maybe$map,
							function (msg) {
								return {
									ctor: '::',
									_0: _debois$elm_mdl$Material_Options$onBlur(msg),
									_1: {ctor: '[]'}
								};
							},
							blur_msg)))),
			{ctor: '[]'});
	});
var _user$project$Main$toggle = F5(
	function (num, txt, val, model, msg) {
		return A5(
			_debois$elm_mdl$Material_Toggles$switch,
			_user$project$Main$Mdl,
			{
				ctor: '::',
				_0: num,
				_1: {ctor: '[]'}
			},
			model.mdl,
			{
				ctor: '::',
				_0: _debois$elm_mdl$Material_Toggles$ripple,
				_1: {
					ctor: '::',
					_0: _debois$elm_mdl$Material_Toggles$value(val),
					_1: {
						ctor: '::',
						_0: _debois$elm_mdl$Material_Options$onToggle(msg),
						_1: {
							ctor: '::',
							_0: A2(_debois$elm_mdl$Material_Options$css, 'margin-top', '10px'),
							_1: {ctor: '[]'}
						}
					}
				}
			},
			{
				ctor: '::',
				_0: _elm_lang$html$Html$text(txt),
				_1: {ctor: '[]'}
			});
	});
var _user$project$Main$viewCrd = function (model) {
	return A2(
		_debois$elm_mdl$Material_Grid$grid,
		{ctor: '[]'},
		{
			ctor: '::',
			_0: A2(
				_debois$elm_mdl$Material_Grid$cell,
				{
					ctor: '::',
					_0: A2(_debois$elm_mdl$Material_Grid$size, _debois$elm_mdl$Material_Grid$All, 2),
					_1: {ctor: '[]'}
				},
				{
					ctor: '::',
					_0: A5(
						_debois$elm_mdl$Material_Textfield$render,
						_user$project$Main$Mdl,
						{
							ctor: '::',
							_0: 1,
							_1: {ctor: '[]'}
						},
						model.mdl,
						{
							ctor: '::',
							_0: _debois$elm_mdl$Material_Textfield$label('f (Hz)'),
							_1: {
								ctor: '::',
								_0: _debois$elm_mdl$Material_Textfield$floatingLabel,
								_1: {
									ctor: '::',
									_0: A2(_debois$elm_mdl$Material_Options$css, 'width', '75px'),
									_1: {
										ctor: '::',
										_0: _debois$elm_mdl$Material_Textfield$maxlength(15),
										_1: {
											ctor: '::',
											_0: _debois$elm_mdl$Material_Textfield$value(
												A2(_user$project$Main$getNumericField, 0, model.crd.cvt.rate)),
											_1: {
												ctor: '::',
												_0: _debois$elm_mdl$Material_Options$onInput(
													function (_p25) {
														return _user$project$Main$Crd(
															_user$project$Crd$UpdateFrequency(_p25));
													}),
												_1: {
													ctor: '::',
													_0: _debois$elm_mdl$Material_Options$onBlur(_user$project$Main$SendCrdFrequency),
													_1: {ctor: '[]'}
												}
											}
										}
									}
								}
							}
						},
						{ctor: '[]'}),
					_1: {
						ctor: '::',
						_0: A5(
							_debois$elm_mdl$Material_Textfield$render,
							_user$project$Main$Mdl,
							{
								ctor: '::',
								_0: 2,
								_1: {ctor: '[]'}
							},
							model.mdl,
							{
								ctor: '::',
								_0: _debois$elm_mdl$Material_Textfield$label('Samples per Cycle'),
								_1: {
									ctor: '::',
									_0: _debois$elm_mdl$Material_Textfield$floatingLabel,
									_1: {
										ctor: '::',
										_0: A2(_debois$elm_mdl$Material_Options$css, 'width', '75px'),
										_1: {
											ctor: '::',
											_0: _debois$elm_mdl$Material_Textfield$maxlength(4),
											_1: {
												ctor: '::',
												_0: _debois$elm_mdl$Material_Textfield$value(
													_elm_lang$core$Basics$toString(model.crd.cvt.dc)),
												_1: {
													ctor: '::',
													_0: _debois$elm_mdl$Material_Options$onInput(
														function (_p26) {
															return _user$project$Main$Crd(
																_user$project$Crd$UpdateDutyCycle(_p26));
														}),
													_1: {
														ctor: '::',
														_0: _debois$elm_mdl$Material_Options$onBlur(_user$project$Main$SendCrdSampleRate),
														_1: {ctor: '[]'}
													}
												}
											}
										}
									}
								}
							},
							{ctor: '[]'}),
						_1: {
							ctor: '::',
							_0: A5(_user$project$Main$toggle, 11, 'Power', model.crd.cvt.power, model, _user$project$Main$ToggleCrdPower),
							_1: {ctor: '[]'}
						}
					}
				}),
			_1: {
				ctor: '::',
				_0: A2(
					_debois$elm_mdl$Material_Grid$cell,
					{
						ctor: '::',
						_0: A2(_debois$elm_mdl$Material_Grid$size, _debois$elm_mdl$Material_Grid$Desktop, 10),
						_1: {
							ctor: '::',
							_0: A2(_debois$elm_mdl$Material_Grid$size, _debois$elm_mdl$Material_Grid$Tablet, 6),
							_1: {ctor: '[]'}
						}
					},
					{
						ctor: '::',
						_0: A2(
							_debois$elm_mdl$Material_Grid$grid,
							{ctor: '[]'},
							{
								ctor: '::',
								_0: A2(
									_debois$elm_mdl$Material_Grid$cell,
									{
										ctor: '::',
										_0: A2(_debois$elm_mdl$Material_Grid$size, _debois$elm_mdl$Material_Grid$All, 12),
										_1: {ctor: '[]'}
									},
									{
										ctor: '::',
										_0: A2(
											_debois$elm_mdl$Material_Table$table,
											{ctor: '[]'},
											{
												ctor: '::',
												_0: A2(
													_debois$elm_mdl$Material_Table$thead,
													{ctor: '[]'},
													{
														ctor: '::',
														_0: A2(
															_debois$elm_mdl$Material_Table$tr,
															{ctor: '[]'},
															{
																ctor: '::',
																_0: A2(
																	_debois$elm_mdl$Material_Table$th,
																	{ctor: '[]'},
																	{
																		ctor: '::',
																		_0: _elm_lang$html$Html$text('ID'),
																		_1: {ctor: '[]'}
																	}),
																_1: {
																	ctor: '::',
																	_0: A2(
																		_debois$elm_mdl$Material_Table$th,
																		{ctor: '[]'},
																		{
																			ctor: '::',
																			_0: A2(
																				_elm_lang$html$Html$span,
																				{
																					ctor: '::',
																					_0: A2(
																						_elm_lang$html$Html_Attributes$property,
																						'innerHTML',
																						_elm_lang$core$Json_Encode$string('&tau;')),
																					_1: {ctor: '[]'}
																				},
																				{ctor: '[]'}),
																			_1: {ctor: '[]'}
																		}),
																	_1: {
																		ctor: '::',
																		_0: A2(
																			_debois$elm_mdl$Material_Table$th,
																			{ctor: '[]'},
																			{
																				ctor: '::',
																				_0: A2(
																					_elm_lang$html$Html$span,
																					{
																						ctor: '::',
																						_0: A2(
																							_elm_lang$html$Html_Attributes$property,
																							'innerHTML',
																							_elm_lang$core$Json_Encode$string('&tau;<sub>0</sub>')),
																						_1: {ctor: '[]'}
																					},
																					{ctor: '[]'}),
																				_1: {ctor: '[]'}
																			}),
																		_1: {
																			ctor: '::',
																			_0: A2(
																				_debois$elm_mdl$Material_Table$th,
																				{ctor: '[]'},
																				{
																					ctor: '::',
																					_0: A2(
																						_elm_lang$html$Html$span,
																						{
																							ctor: '::',
																							_0: A2(
																								_elm_lang$html$Html_Attributes$property,
																								'innerHTML',
																								_elm_lang$core$Json_Encode$string('&tau;<sub>0</sub>\'')),
																							_1: {ctor: '[]'}
																						},
																						{ctor: '[]'}),
																					_1: {ctor: '[]'}
																				}),
																			_1: {
																				ctor: '::',
																				_0: A2(
																					_debois$elm_mdl$Material_Table$th,
																					{ctor: '[]'},
																					{
																						ctor: '::',
																						_0: A2(
																							_elm_lang$html$Html$span,
																							{
																								ctor: '::',
																								_0: A2(
																									_elm_lang$html$Html_Attributes$property,
																									'innerHTML',
																									_elm_lang$core$Json_Encode$string('&tau;\'')),
																								_1: {ctor: '[]'}
																							},
																							{ctor: '[]'}),
																						_1: {ctor: '[]'}
																					}),
																				_1: {
																					ctor: '::',
																					_0: A2(
																						_debois$elm_mdl$Material_Table$th,
																						{ctor: '[]'},
																						{
																							ctor: '::',
																							_0: A2(
																								_elm_lang$html$Html$span,
																								{
																									ctor: '::',
																									_0: A2(
																										_elm_lang$html$Html_Attributes$property,
																										'innerHTML',
																										_elm_lang$core$Json_Encode$string('&sigma;')),
																									_1: {ctor: '[]'}
																								},
																								{ctor: '[]'}),
																							_1: {ctor: '[]'}
																						}),
																					_1: {
																						ctor: '::',
																						_0: A2(
																							_debois$elm_mdl$Material_Table$th,
																							{ctor: '[]'},
																							{
																								ctor: '::',
																								_0: _elm_lang$html$Html$text('max'),
																								_1: {ctor: '[]'}
																							}),
																						_1: {ctor: '[]'}
																					}
																				}
																			}
																		}
																	}
																}
															}),
														_1: {
															ctor: '::',
															_0: A2(
																_debois$elm_mdl$Material_Table$tr,
																{ctor: '[]'},
																{
																	ctor: '::',
																	_0: A2(
																		_debois$elm_mdl$Material_Table$th,
																		{ctor: '[]'},
																		{ctor: '[]'}),
																	_1: {
																		ctor: '::',
																		_0: A2(
																			_debois$elm_mdl$Material_Table$th,
																			{ctor: '[]'},
																			{
																				ctor: '::',
																				_0: _elm_lang$html$Html$text('(us)'),
																				_1: {ctor: '[]'}
																			}),
																		_1: {
																			ctor: '::',
																			_0: A2(
																				_debois$elm_mdl$Material_Table$th,
																				{ctor: '[]'},
																				{
																					ctor: '::',
																					_0: _elm_lang$html$Html$text('(us)'),
																					_1: {ctor: '[]'}
																				}),
																			_1: {
																				ctor: '::',
																				_0: A2(
																					_debois$elm_mdl$Material_Table$th,
																					{ctor: '[]'},
																					{
																						ctor: '::',
																						_0: _elm_lang$html$Html$text('(us)'),
																						_1: {ctor: '[]'}
																					}),
																				_1: {
																					ctor: '::',
																					_0: A2(
																						_debois$elm_mdl$Material_Table$th,
																						{ctor: '[]'},
																						{
																							ctor: '::',
																							_0: _elm_lang$html$Html$text('(us)'),
																							_1: {ctor: '[]'}
																						}),
																					_1: {
																						ctor: '::',
																						_0: A2(
																							_debois$elm_mdl$Material_Table$th,
																							{ctor: '[]'},
																							{
																								ctor: '::',
																								_0: A2(
																									_elm_lang$html$Html$span,
																									{
																										ctor: '::',
																										_0: A2(
																											_elm_lang$html$Html_Attributes$property,
																											'innerHTML',
																											_elm_lang$core$Json_Encode$string('Mm<sup>-1</sup>')),
																										_1: {ctor: '[]'}
																									},
																									{ctor: '[]'}),
																								_1: {ctor: '[]'}
																							}),
																						_1: {
																							ctor: '::',
																							_0: A2(
																								_debois$elm_mdl$Material_Table$th,
																								{ctor: '[]'},
																								{
																									ctor: '::',
																									_0: _elm_lang$html$Html$text('(a.u.)'),
																									_1: {ctor: '[]'}
																								}),
																							_1: {ctor: '[]'}
																						}
																					}
																				}
																			}
																		}
																	}
																}),
															_1: {ctor: '[]'}
														}
													}),
												_1: {
													ctor: '::',
													_0: A2(
														_debois$elm_mdl$Material_Table$tbody,
														{ctor: '[]'},
														A2(
															_elm_lang$core$List$map,
															function (cell) {
																var data = _elm_lang$core$Tuple$second(cell);
																var id = _elm_lang$core$Tuple$first(cell);
																return A2(
																	_debois$elm_mdl$Material_Table$tr,
																	{ctor: '[]'},
																	{
																		ctor: '::',
																		_0: A2(
																			_debois$elm_mdl$Material_Table$td,
																			{ctor: '[]'},
																			{
																				ctor: '::',
																				_0: _elm_lang$html$Html$text(id),
																				_1: {ctor: '[]'}
																			}),
																		_1: {
																			ctor: '::',
																			_0: A2(
																				_debois$elm_mdl$Material_Table$td,
																				{ctor: '[]'},
																				{
																					ctor: '::',
																					_0: _elm_lang$html$Html$text(
																						_user$project$Main$printableNumeric(data.tau)),
																					_1: {ctor: '[]'}
																				}),
																			_1: {
																				ctor: '::',
																				_0: A2(
																					_debois$elm_mdl$Material_Table$td,
																					{ctor: '[]'},
																					{
																						ctor: '::',
																						_0: _elm_lang$html$Html$text(
																							_user$project$Main$printableNumeric(data.tau0)),
																						_1: {ctor: '[]'}
																					}),
																				_1: {
																					ctor: '::',
																					_0: A2(
																						_debois$elm_mdl$Material_Table$td,
																						{ctor: '[]'},
																						{
																							ctor: '::',
																							_0: _elm_lang$html$Html$text(
																								_user$project$Main$printableNumeric(data.tau0corr)),
																							_1: {ctor: '[]'}
																						}),
																					_1: {
																						ctor: '::',
																						_0: A2(
																							_debois$elm_mdl$Material_Table$td,
																							{ctor: '[]'},
																							{
																								ctor: '::',
																								_0: _elm_lang$html$Html$text(
																									_user$project$Main$printableNumeric(data.tauCorrected)),
																								_1: {ctor: '[]'}
																							}),
																						_1: {
																							ctor: '::',
																							_0: A2(
																								_debois$elm_mdl$Material_Table$td,
																								{ctor: '[]'},
																								{
																									ctor: '::',
																									_0: _elm_lang$html$Html$text(
																										_user$project$Main$printableNumeric(data.extinction)),
																									_1: {ctor: '[]'}
																								}),
																							_1: {
																								ctor: '::',
																								_0: A2(
																									_debois$elm_mdl$Material_Table$td,
																									{ctor: '[]'},
																									{
																										ctor: '::',
																										_0: _elm_lang$html$Html$text(
																											_user$project$Main$printableNumeric(data.max)),
																										_1: {ctor: '[]'}
																									}),
																								_1: {ctor: '[]'}
																							}
																						}
																					}
																				}
																			}
																		}
																	});
															},
															A3(
																_elm_lang$core$List$map2,
																F2(
																	function (v0, v1) {
																		return {ctor: '_Tuple2', _0: v0, _1: v1};
																	}),
																model.crd.cvt.labels,
																_elm_lang$core$Array$toList(model.crd.data)))),
													_1: {ctor: '[]'}
												}
											}),
										_1: {ctor: '[]'}
									}),
								_1: {
									ctor: '::',
									_0: A2(
										_debois$elm_mdl$Material_Grid$cell,
										{
											ctor: '::',
											_0: A2(_debois$elm_mdl$Material_Grid$size, _debois$elm_mdl$Material_Grid$All, 12),
											_1: {ctor: '[]'}
										},
										{
											ctor: '::',
											_0: A4(
												_user$project$Main$timeData,
												model,
												model.crdRange,
												_user$project$Main$getRingdownData(model),
												model.crdPlotData),
											_1: {ctor: '[]'}
										}),
									_1: {
										ctor: '::',
										_0: A2(
											_debois$elm_mdl$Material_Grid$cell,
											{
												ctor: '::',
												_0: A2(_debois$elm_mdl$Material_Grid$size, _debois$elm_mdl$Material_Grid$All, 12),
												_1: {ctor: '[]'}
											},
											{
												ctor: '::',
												_0: A5(
													_debois$elm_mdl$Material_Toggles$checkbox,
													_user$project$Main$Mdl,
													{
														ctor: '::',
														_0: 100,
														_1: {ctor: '[]'}
													},
													model.mdl,
													{
														ctor: '::',
														_0: _debois$elm_mdl$Material_Toggles$value(
															A2(
																_elm_lang$core$Maybe$withDefault,
																false,
																A2(_elm_community$list_extra$List_Extra$getAt, 0, model.crdPlotData))),
														_1: {
															ctor: '::',
															_0: _debois$elm_mdl$Material_Toggles$ripple,
															_1: {
																ctor: '::',
																_0: _debois$elm_mdl$Material_Options$onToggle(
																	_user$project$Main$ToggleCrdPlot(0)),
																_1: {
																	ctor: '::',
																	_0: A2(_debois$elm_mdl$Material_Options$css, 'color', 'blue'),
																	_1: {ctor: '[]'}
																}
															}
														}
													},
													{
														ctor: '::',
														_0: _elm_lang$html$Html$text('Channel 1'),
														_1: {ctor: '[]'}
													}),
												_1: {
													ctor: '::',
													_0: A5(
														_debois$elm_mdl$Material_Toggles$checkbox,
														_user$project$Main$Mdl,
														{
															ctor: '::',
															_0: 101,
															_1: {ctor: '[]'}
														},
														model.mdl,
														{
															ctor: '::',
															_0: _debois$elm_mdl$Material_Toggles$value(
																A2(
																	_elm_lang$core$Maybe$withDefault,
																	false,
																	A2(_elm_community$list_extra$List_Extra$getAt, 1, model.crdPlotData))),
															_1: {
																ctor: '::',
																_0: _debois$elm_mdl$Material_Options$onToggle(
																	_user$project$Main$ToggleCrdPlot(1)),
																_1: {
																	ctor: '::',
																	_0: _debois$elm_mdl$Material_Toggles$ripple,
																	_1: {
																		ctor: '::',
																		_0: A2(_debois$elm_mdl$Material_Options$css, 'color', 'red'),
																		_1: {ctor: '[]'}
																	}
																}
															}
														},
														{
															ctor: '::',
															_0: _elm_lang$html$Html$text('Channel 2'),
															_1: {ctor: '[]'}
														}),
													_1: {
														ctor: '::',
														_0: A5(
															_debois$elm_mdl$Material_Textfield$render,
															_user$project$Main$Mdl,
															{
																ctor: '::',
																_0: 103,
																_1: {ctor: '[]'}
															},
															model.mdl,
															{
																ctor: '::',
																_0: _debois$elm_mdl$Material_Textfield$floatingLabel,
																_1: {
																	ctor: '::',
																	_0: _debois$elm_mdl$Material_Textfield$value(
																		_elm_lang$core$Basics$toString(model.crdRange.xmin)),
																	_1: {
																		ctor: '::',
																		_0: _debois$elm_mdl$Material_Options$onInput(
																			_user$project$Main$UpdateCrdRange('xmin')),
																		_1: {
																			ctor: '::',
																			_0: A2(_debois$elm_mdl$Material_Options$css, 'width', '125px'),
																			_1: {
																				ctor: '::',
																				_0: _debois$elm_mdl$Material_Textfield$maxlength(15),
																				_1: {
																					ctor: '::',
																					_0: _debois$elm_mdl$Material_Textfield$label('xMin'),
																					_1: {ctor: '[]'}
																				}
																			}
																		}
																	}
																}
															},
															{ctor: '[]'}),
														_1: {
															ctor: '::',
															_0: A5(
																_debois$elm_mdl$Material_Textfield$render,
																_user$project$Main$Mdl,
																{
																	ctor: '::',
																	_0: 103,
																	_1: {ctor: '[]'}
																},
																model.mdl,
																{
																	ctor: '::',
																	_0: _debois$elm_mdl$Material_Textfield$floatingLabel,
																	_1: {
																		ctor: '::',
																		_0: _debois$elm_mdl$Material_Textfield$value(
																			_elm_lang$core$Basics$toString(model.crdRange.xmax)),
																		_1: {
																			ctor: '::',
																			_0: _debois$elm_mdl$Material_Options$onInput(
																				_user$project$Main$UpdateCrdRange('xmax')),
																			_1: {
																				ctor: '::',
																				_0: A2(_debois$elm_mdl$Material_Options$css, 'width', '125px'),
																				_1: {
																					ctor: '::',
																					_0: _debois$elm_mdl$Material_Textfield$maxlength(15),
																					_1: {
																						ctor: '::',
																						_0: _debois$elm_mdl$Material_Textfield$label('xMax'),
																						_1: {ctor: '[]'}
																					}
																				}
																			}
																		}
																	}
																},
																{ctor: '[]'}),
															_1: {
																ctor: '::',
																_0: A5(
																	_debois$elm_mdl$Material_Textfield$render,
																	_user$project$Main$Mdl,
																	{
																		ctor: '::',
																		_0: 104,
																		_1: {ctor: '[]'}
																	},
																	model.mdl,
																	{
																		ctor: '::',
																		_0: _debois$elm_mdl$Material_Textfield$floatingLabel,
																		_1: {
																			ctor: '::',
																			_0: _debois$elm_mdl$Material_Textfield$value(
																				_elm_lang$core$Basics$toString(model.crdRange.ymin)),
																			_1: {
																				ctor: '::',
																				_0: _debois$elm_mdl$Material_Options$onInput(
																					_user$project$Main$UpdateCrdRange('ymin')),
																				_1: {
																					ctor: '::',
																					_0: A2(_debois$elm_mdl$Material_Options$css, 'width', '125px'),
																					_1: {
																						ctor: '::',
																						_0: _debois$elm_mdl$Material_Textfield$maxlength(15),
																						_1: {
																							ctor: '::',
																							_0: _debois$elm_mdl$Material_Textfield$label('yMin'),
																							_1: {ctor: '[]'}
																						}
																					}
																				}
																			}
																		}
																	},
																	{ctor: '[]'}),
																_1: {
																	ctor: '::',
																	_0: A5(
																		_debois$elm_mdl$Material_Textfield$render,
																		_user$project$Main$Mdl,
																		{
																			ctor: '::',
																			_0: 105,
																			_1: {ctor: '[]'}
																		},
																		model.mdl,
																		{
																			ctor: '::',
																			_0: _debois$elm_mdl$Material_Textfield$floatingLabel,
																			_1: {
																				ctor: '::',
																				_0: _debois$elm_mdl$Material_Textfield$value(
																					_elm_lang$core$Basics$toString(model.crdRange.ymax)),
																				_1: {
																					ctor: '::',
																					_0: _debois$elm_mdl$Material_Options$onInput(
																						_user$project$Main$UpdateCrdRange('ymax')),
																					_1: {
																						ctor: '::',
																						_0: A2(_debois$elm_mdl$Material_Options$css, 'width', '125px'),
																						_1: {
																							ctor: '::',
																							_0: _debois$elm_mdl$Material_Textfield$maxlength(15),
																							_1: {
																								ctor: '::',
																								_0: _debois$elm_mdl$Material_Textfield$label('yMax'),
																								_1: {ctor: '[]'}
																							}
																						}
																					}
																				}
																			}
																		},
																		{ctor: '[]'}),
																	_1: {
																		ctor: '::',
																		_0: A4(_user$project$Main$button, 106, 'Autoscale 1x', model, _user$project$Main$UpdateCrdScaling),
																		_1: {ctor: '[]'}
																	}
																}
															}
														}
													}
												}
											}),
										_1: {
											ctor: '::',
											_0: A2(
												_debois$elm_mdl$Material_Grid$cell,
												{
													ctor: '::',
													_0: A2(_debois$elm_mdl$Material_Grid$size, _debois$elm_mdl$Material_Grid$All, 12),
													_1: {ctor: '[]'}
												},
												{
													ctor: '::',
													_0: A2(_user$project$Main$plotData, model.crdRunningData, 0),
													_1: {ctor: '[]'}
												}),
											_1: {ctor: '[]'}
										}
									}
								}
							}),
						_1: {ctor: '[]'}
					}),
				_1: {ctor: '[]'}
			}
		});
};
var _user$project$Main$PhotoDiodDiode = {ctor: 'PhotoDiodDiode'};
var _user$project$Main$MicTime = {ctor: 'MicTime'};
var _user$project$Main$MicFreq = {ctor: 'MicFreq'};
var _user$project$Main$update = F2(
	function (msg, model) {
		var _p27 = msg;
		switch (_p27.ctor) {
			case 'Mdl':
				return A3(_debois$elm_mdl$Material$update, _user$project$Main$Mdl, _p27._0, model);
			case 'SelectTab':
				var new_model = _elm_lang$core$Native_Utils.update(
					model,
					{selectedTab: _p27._0});
				return {
					ctor: '_Tuple2',
					_0: new_model,
					_1: _user$project$Main$updateWaveforms(new_model)
				};
			case 'CheckCvtData':
				return {
					ctor: '_Tuple2',
					_0: model,
					_1: A2(_user$project$Main$getCvtData, model, '0')
				};
			case 'ForceCvtCheck':
				return {
					ctor: '_Tuple2',
					_0: model,
					_1: A2(_user$project$Main$getCvtData, model, '1')
				};
			case 'CheckData':
				return {
					ctor: '_Tuple2',
					_0: model,
					_1: _user$project$Main$getData(model)
				};
			case 'Network':
				var new_model = A2(
					_user$project$Main$asNetworkIn,
					model,
					A2(_user$project$Network$update, _p27._0, model.network));
				var ip = new_model.network.ip;
				var port_ = new_model.network.port_;
				return {
					ctor: '_Tuple2',
					_0: new_model,
					_1: _user$project$Network$updateIpConfig(
						{
							ctor: '::',
							_0: ip,
							_1: {
								ctor: '::',
								_0: port_,
								_1: {ctor: '[]'}
							}
						})
				};
			case 'Pas':
				var new_model = A2(
					_user$project$Main$asPasIn,
					model,
					A2(_user$project$Pas$update, _p27._0, model.pas));
				return {ctor: '_Tuple2', _0: new_model, _1: _elm_lang$core$Platform_Cmd$none};
			case 'Crd':
				var new_model = A2(
					_user$project$Main$asCrdIn,
					model,
					A2(_user$project$Crd$update, _p27._0, model.crd));
				return {ctor: '_Tuple2', _0: new_model, _1: _elm_lang$core$Platform_Cmd$none};
			case 'SaveData':
				var new_model = _elm_lang$core$Native_Utils.update(
					model,
					{save: !model.save});
				return {
					ctor: '_Tuple2',
					_0: new_model,
					_1: _user$project$Main$toggleSave(new_model)
				};
			case 'StopServer':
				return {
					ctor: '_Tuple2',
					_0: model,
					_1: _user$project$Main$shutdownSystem(model)
				};
			case 'ToggleO3':
				var new_model = A2(
					_user$project$Main$asCvtIn,
					model,
					A2(
						_user$project$GcrdTypes$asCalibrationIn,
						model.cvt,
						_user$project$GcrdTypes$toggleO3AddPosition(model.cvt.cal_state)));
				var val = new_model.cvt.cal_state.o3_add ? '1' : '0';
				return {
					ctor: '_Tuple2',
					_0: new_model,
					_1: A2(_user$project$Main$toggleO3, new_model, val)
				};
			case 'ToggleO2':
				var new_model = A2(
					_user$project$Main$asCvtIn,
					model,
					A2(
						_user$project$GcrdTypes$asCalibrationIn,
						model.cvt,
						_user$project$GcrdTypes$toggleO2AddPosition(model.cvt.cal_state)));
				var val = new_model.cvt.cal_state.o2_add ? '1' : '0';
				return {
					ctor: '_Tuple2',
					_0: new_model,
					_1: A2(_user$project$Main$toggleO2, new_model, val)
				};
			case 'ToggleFilter':
				var new_model = A2(
					_user$project$Main$asCvtIn,
					model,
					A2(
						_user$project$GcrdTypes$asFilterIn,
						model.cvt,
						_user$project$GcrdTypes$toggleFilterPosition(model.cvt.filter)));
				return {
					ctor: '_Tuple2',
					_0: new_model,
					_1: _user$project$Main$toggleFilter(new_model)
				};
			case 'TogglePump':
				var new_model = A2(
					_user$project$Main$asCvtIn,
					model,
					_user$project$GcrdTypes$switchPump(model.cvt));
				return {
					ctor: '_Tuple2',
					_0: new_model,
					_1: _user$project$Main$togglePump(new_model)
				};
			case 'ToggleUVLamp':
				var val = model.cvt.cal_state.uv_lamp ? '0' : '1';
				var new_model = A2(
					_user$project$Main$asCvtIn,
					model,
					A2(
						_user$project$GcrdTypes$asCalibrationIn,
						model.cvt,
						_user$project$GcrdTypes$toggleUVLampPosition(model.cvt.cal_state)));
				return {
					ctor: '_Tuple2',
					_0: new_model,
					_1: A2(_user$project$Main$toggleUV, new_model, val)
				};
			case 'ToggleUSB':
				return {ctor: '_Tuple2', _0: model, _1: _elm_lang$core$Platform_Cmd$none};
			case 'GetCVT':
				if (_p27._0.ctor === 'Ok') {
					var _p28 = _p27._0._0;
					var dev_cvt = A2(
						_elm_lang$core$Result$withDefault,
						_user$project$Devices_Device$defaultDeviceDict,
						A2(_elm_lang$core$Json_Decode$decodeString, _user$project$Devices_Device$decodeDeviceCvt, _p28));
					var a_model = A2(
						_user$project$Main$asAlicatIn,
						model,
						A2(_user$project$Devices_Alicat$insertAlicatDev, dev_cvt, model.alicats));
					var v_model = A2(
						_user$project$Main$asVaisalaIn,
						a_model,
						A2(_user$project$Devices_Vaisala$insertVaisalaDev, dev_cvt, a_model.vaisalas));
					var p_model = A2(
						_user$project$Main$asPptIn,
						v_model,
						A2(_user$project$Devices_Ppt$insertPptDev, dev_cvt, v_model.ppts));
					var new_model = A2(
						_user$project$Main$asCvtIn,
						p_model,
						A2(
							_elm_lang$core$Result$withDefault,
							_user$project$GcrdTypes$defaultCvtData,
							A2(
								_elm_lang$core$Json_Decode$decodeString,
								_user$project$GcrdTypes$decodeCvt(p_model.cvt),
								_p28)));
					var model_with_crd = A2(
						_user$project$Main$asCrdIn,
						new_model,
						A2(
							_user$project$Crd$asCvtIn,
							new_model.crd,
							A3(_user$project$Crd$retrieveCrdCvt, 'crd', _p28, new_model.crd.cvt)));
					var model_with_pas = A2(
						_user$project$Main$asPasIn,
						model_with_crd,
						A2(
							_user$project$Pas$asCvtIn,
							model_with_crd.pas,
							A3(_user$project$Pas$retrievePasCvt, 'pas', _p28, model_with_crd.pas.cvt)));
					var nmodel = _elm_lang$core$Native_Utils.eq(_p28, '{}') ? model : model_with_pas;
					return {ctor: '_Tuple2', _0: nmodel, _1: _elm_lang$core$Platform_Cmd$none};
				} else {
					var data_ = A2(
						_elm_lang$core$Basics_ops['++'],
						A2(_elm_lang$core$Debug$log, 'CVT-Error', 'There was an error contacting '),
						_user$project$Network$buildAddress(model.network));
					return {ctor: '_Tuple2', _0: model, _1: _elm_lang$core$Platform_Cmd$none};
				}
			case 'GetData':
				if (_p27._0.ctor === 'Ok') {
					var _p29 = _p27._0._0;
					var rCrdData = A2(
						_elm_lang$core$Maybe$withDefault,
						_user$project$Crd$CrdsCell(0)(0)(0)(0)(0)(0)(0)(0)(0)(
							{
								ctor: '::',
								_0: {
									ctor: '::',
									_0: 0,
									_1: {ctor: '[]'}
								},
								_1: {ctor: '[]'}
							}),
						A2(_elm_lang$core$Array$get, 1, model.crd.data));
					var cListData = {
						ctor: '::',
						_0: rCrdData.tau,
						_1: {
							ctor: '::',
							_0: rCrdData.tau0,
							_1: {
								ctor: '::',
								_0: rCrdData.max,
								_1: {ctor: '[]'}
							}
						}
					};
					var rPasData = A2(
						_elm_lang$core$Maybe$withDefault,
						_user$project$Pas$PasCell(0)(0)(0)(
							{
								ctor: '::',
								_0: 0,
								_1: {
									ctor: '::',
									_0: 0,
									_1: {ctor: '[]'}
								}
							})(0)(0)(
							{
								ctor: '::',
								_0: 0,
								_1: {ctor: '[]'}
							})(
							{
								ctor: '::',
								_0: 0,
								_1: {ctor: '[]'}
							})(
							{
								ctor: '::',
								_0: 0,
								_1: {ctor: '[]'}
							})(
							{
								ctor: '::',
								_0: 0,
								_1: {ctor: '[]'}
							})(0),
						A2(_elm_lang$core$Array$get, 1, model.pas.data.cell));
					var listData = {
						ctor: '::',
						_0: rPasData.resonant_frequency,
						_1: {
							ctor: '::',
							_0: rPasData.integrated_area,
							_1: {
								ctor: '::',
								_0: rPasData.laserRMS,
								_1: {
									ctor: '::',
									_0: rPasData.absorption,
									_1: {ctor: '[]'}
								}
							}
						}
					};
					var crd_cell_data = A3(_user$project$Crd$retrieveCrdData, 'CellData', _p29, model.crd.data);
					var new_model = A2(
						_user$project$Main$asCrdIn,
						model,
						A2(_user$project$Crd$asDataIn, model.crd, crd_cell_data));
					var newest_model = A2(
						_user$project$Main$asAlicatIn,
						new_model,
						A2(_user$project$Devices_Alicat$getAlicatData, new_model.alicats, _p29));
					var newestest_model = A2(
						_user$project$Main$asVaisalaIn,
						newest_model,
						A2(_user$project$Devices_Vaisala$getVaisalaData, newest_model.vaisalas, _p29));
					var p_model = A2(
						_user$project$Main$asPptIn,
						newestest_model,
						A2(_user$project$Devices_Ppt$getPptData, newestest_model.ppts, _p29));
					var newer_model = A2(
						_user$project$Main$asDataIn,
						p_model,
						A2(
							_elm_lang$core$Result$withDefault,
							p_model.genData,
							A2(
								_elm_lang$core$Json_Decode$decodeString,
								A2(_elm_lang$core$Json_Decode$field, 'general', _user$project$GcrdTypes$decodeData),
								_p29)));
					var pas_model = A2(
						_user$project$Main$asPasIn,
						newer_model,
						A4(
							_user$project$Pas$truncateFrequencyData,
							1,
							1200,
							1500,
							A4(
								_user$project$Pas$truncateFrequencyData,
								0,
								1200,
								1500,
								A3(_user$project$Pas$retrievePasData, 'PAS', _p29, newer_model.pas))));
					var n_model = A2(
						_user$project$Main$asRunningDataIn,
						pas_model,
						A3(_user$project$Main$addDataToList, 10, listData, pas_model.runningData));
					var nn_model = A2(
						_user$project$Main$asCrdRunningDataIn,
						n_model,
						A3(_user$project$Main$addDataToList, 100, cListData, n_model.crdRunningData));
					var nn_msgs = _elm_lang$core$Native_Utils.update(
						nn_model,
						{
							currentMsgList: A2(_elm_lang$core$List$append, nn_model.currentMsgList, nn_model.genData.msg)
						});
					return {ctor: '_Tuple2', _0: nn_msgs, _1: _elm_lang$core$Platform_Cmd$none};
				} else {
					return {ctor: '_Tuple2', _0: model, _1: _elm_lang$core$Platform_Cmd$none};
				}
			case 'HandleGeneric':
				if (_p27._0.ctor === 'Ok') {
					return {ctor: '_Tuple2', _0: model, _1: _elm_lang$core$Platform_Cmd$none};
				} else {
					return {ctor: '_Tuple2', _0: model, _1: _elm_lang$core$Platform_Cmd$none};
				}
			case 'InitializeNetwork':
				var _p30 = _p27._0;
				var p = _elm_lang$core$Tuple$second(_p30);
				var ip = _elm_lang$core$Tuple$first(_p30);
				var network = model.network;
				var network_ = _elm_lang$core$Native_Utils.update(
					network,
					{ip: ip});
				var new_model = _elm_lang$core$Native_Utils.update(
					model,
					{
						network: _elm_lang$core$Native_Utils.update(
							network_,
							{port_: p})
					});
				return {
					ctor: '_Tuple2',
					_0: new_model,
					_1: A2(_user$project$Main$getCvtData, new_model, '1')
				};
			case 'ToggleSpeaker':
				var _p33 = _p27._0;
				var ncell = _elm_lang$core$Native_Utils.eq(_p33, 0) ? '0' : '1';
				var new_model = function () {
					var _p31 = _p33;
					switch (_p31) {
						case 0:
							return A2(
								_user$project$Main$asPasIn,
								model,
								A2(
									_user$project$Pas$asCvtIn,
									model.pas,
									_user$project$Pas$toggleSpeaker0Position(model.pas.cvt)));
						case 1:
							return A2(
								_user$project$Main$asPasIn,
								model,
								A2(
									_user$project$Pas$asCvtIn,
									model.pas,
									_user$project$Pas$toggleSpeaker1Position(model.pas.cvt)));
						default:
							return model;
					}
				}();
				var val = function () {
					var _p32 = _p33;
					switch (_p32) {
						case 0:
							return new_model.pas.cvt.speaker_0 ? '1' : '0';
						case 1:
							return new_model.pas.cvt.speaker_1 ? '1' : '0';
						default:
							return '1';
					}
				}();
				return {
					ctor: '_Tuple2',
					_0: new_model,
					_1: A3(_user$project$Main$toggleSpk, new_model, val, ncell)
				};
			case 'UpdateMod0':
				var f_ = A2(
					_elm_lang$core$Result$withDefault,
					1350,
					_elm_lang$core$String$toInt(_p27._0));
				var new_model = A2(
					_user$project$Main$asPasIn,
					model,
					A2(
						_user$project$Pas$asCvtIn,
						model.pas,
						A2(_user$project$Pas$setFrequency0, f_, model.pas.cvt)));
				return {ctor: '_Tuple2', _0: new_model, _1: _elm_lang$core$Platform_Cmd$none};
			case 'UpdateMod1':
				var f_ = A2(
					_elm_lang$core$Result$withDefault,
					1350,
					_elm_lang$core$String$toInt(_p27._0));
				var new_model = A2(
					_user$project$Main$asPasIn,
					model,
					A2(
						_user$project$Pas$asCvtIn,
						model.pas,
						A2(_user$project$Pas$setFrequency1, f_, model.pas.cvt)));
				return {ctor: '_Tuple2', _0: new_model, _1: _elm_lang$core$Platform_Cmd$none};
			case 'SendModulation':
				var _p34 = _p27._0;
				var freq = _elm_lang$core$Native_Utils.eq(_p34, '1') ? model.pas.cvt.fmod_1 : model.pas.cvt.fmod_0;
				var f = _elm_lang$core$Basics$toString(freq);
				var index = A2(
					_elm_lang$core$Result$withDefault,
					0,
					_elm_lang$core$String$toInt(_p34));
				return {
					ctor: '_Tuple2',
					_0: model,
					_1: A3(_user$project$Main$setCellFrequency, model, _p34, f)
				};
			case 'SendCrdFrequency':
				return {
					ctor: '_Tuple2',
					_0: model,
					_1: _user$project$Main$setCrdFrequency(model)
				};
			case 'SendCrdSampleRate':
				return {
					ctor: '_Tuple2',
					_0: model,
					_1: _user$project$Main$setCrdRate(model)
				};
			case 'ToggleCrdPower':
				var new_model = A2(
					_user$project$Main$asCrdIn,
					model,
					A2(
						_user$project$Crd$asCvtIn,
						model.crd,
						_user$project$Crd$togglePower(model.crd.cvt)));
				var val = new_model.crd.cvt.power ? '1' : '0';
				return {
					ctor: '_Tuple2',
					_0: new_model,
					_1: A2(_user$project$Main$toggleCrdPower, model, val)
				};
			case 'UpdateHeaterSP':
				var _p36 = _p27._1;
				var _p35 = _p27._0;
				switch (_p35.ctor) {
					case 'CrdHeater':
						return {ctor: '_Tuple2', _0: model, _1: _elm_lang$core$Platform_Cmd$none};
					case 'Pas0Heater':
						var new_model = A2(
							_user$project$Main$asPasIn,
							model,
							A2(
								_user$project$Pas$asCvtIn,
								model.pas,
								A2(
									_user$project$Pas$asHeater1In,
									model.pas.cvt,
									A2(_user$project$Pas$setHeaterSP, _p36, model.pas.cvt.heater_0))));
						return {ctor: '_Tuple2', _0: new_model, _1: _elm_lang$core$Platform_Cmd$none};
					default:
						var new_model = A2(
							_user$project$Main$asPasIn,
							model,
							A2(
								_user$project$Pas$asCvtIn,
								model.pas,
								A2(
									_user$project$Pas$asHeater1In,
									model.pas.cvt,
									A2(_user$project$Pas$setHeaterSP, _p36, model.pas.cvt.heater_1))));
						return {ctor: '_Tuple2', _0: new_model, _1: _elm_lang$core$Platform_Cmd$none};
				}
			case 'TogglePasLaserPower':
				var _p38 = _p27._0;
				var c = _elm_lang$core$Basics$toString(_p38);
				var new_model = A2(
					_user$project$Main$asPasIn,
					model,
					A2(
						_user$project$Pas$asCvtIn,
						model.pas,
						A2(_user$project$Pas$toggleLaserPower, _p38, model.pas.cvt)));
				var cmd = function () {
					var _p37 = _p38;
					switch (_p37) {
						case 0:
							var val = new_model.pas.cvt.enable_0 ? '1' : '0';
							return A3(
								_user$project$Main$togglePasLaserPower,
								new_model,
								val,
								_elm_lang$core$Basics$toString(_p38));
						case 1:
							var val = new_model.pas.cvt.enable_1 ? '1' : '0';
							return A3(
								_user$project$Main$togglePasLaserPower,
								new_model,
								val,
								_elm_lang$core$Basics$toString(_p38));
						default:
							return _elm_lang$core$Platform_Cmd$none;
					}
				}();
				return {ctor: '_Tuple2', _0: new_model, _1: cmd};
			case 'ClearMessages':
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Native_Utils.update(
						model,
						{
							currentMsgList: {ctor: '[]'}
						}),
					_1: _elm_lang$core$Platform_Cmd$none
				};
			case 'SendSpkVoltage':
				return {
					ctor: '_Tuple2',
					_0: model,
					_1: _user$project$Main$sendSpkVoltage(model)
				};
			case 'UpdateChirp':
				return {
					ctor: '_Tuple2',
					_0: model,
					_1: _user$project$Main$sendChirp(model)
				};
			case 'ToggleFan':
				var new_model = A2(
					_user$project$Main$asCvtIn,
					model,
					A2(_user$project$GcrdTypes$setFanEnable, !model.cvt.fan, model.cvt));
				var val = new_model.cvt.fan ? '1' : '0';
				return {
					ctor: '_Tuple2',
					_0: new_model,
					_1: A2(_user$project$Main$toggleFan, new_model, val)
				};
			case 'UpdateFanVoltage':
				var new_model = A2(
					_user$project$Main$asCvtIn,
					model,
					A2(_user$project$GcrdTypes$setFanVoltage, _p27._0, model.cvt));
				return {
					ctor: '_Tuple2',
					_0: new_model,
					_1: _user$project$Main$sendFanVoltage(new_model)
				};
			case 'UpdateDevSP':
				var _p39 = _p27._0;
				var dev = A2(
					_elm_lang$core$Maybe$withDefault,
					_user$project$Devices_Device$defaultDevice,
					A2(_elm_lang$core$Dict$get, _p39, model.alicats.cvt));
				var new_dev = A2(_user$project$Devices_Device$setSpIn, _p27._1, dev);
				var new_model = A2(
					_user$project$Main$asAlicatIn,
					model,
					A2(
						_user$project$Devices_Alicat$asCvtIn,
						model.alicats,
						A3(_elm_lang$core$Dict$insert, _p39, new_dev, model.alicats.cvt)));
				return {ctor: '_Tuple2', _0: new_model, _1: _elm_lang$core$Platform_Cmd$none};
			case 'SendDevSP':
				var _p40 = _p27._0;
				var dev = A2(
					_elm_lang$core$Maybe$withDefault,
					_user$project$Devices_Device$defaultDevice,
					A2(_elm_lang$core$Dict$get, _p40, model.alicats.cvt));
				return {
					ctor: '_Tuple2',
					_0: model,
					_1: A3(_user$project$Main$sendDevSp, _p40, dev, model)
				};
			case 'UpdateTime':
				return {
					ctor: '_Tuple2',
					_0: model,
					_1: A2(_user$project$Main$sendNewTime, _p27._0, model)
				};
			case 'SyncTime':
				return {ctor: '_Tuple2', _0: model, _1: _user$project$Main$getCurrentTime};
			case 'TogglePasPlot':
				var _p41 = _p27._0;
				var d = A2(
					_elm_lang$core$Maybe$withDefault,
					false,
					A2(_elm_community$list_extra$List_Extra$getAt, _p41, model.pasPlotData));
				var newModel = _elm_lang$core$Native_Utils.update(
					model,
					{
						pasPlotData: A3(_elm_community$list_extra$List_Extra$setAt, _p41, !d, model.pasPlotData)
					});
				return {ctor: '_Tuple2', _0: newModel, _1: _elm_lang$core$Platform_Cmd$none};
			case 'UpdatePasRange':
				var range = model.pasRange;
				var v = A2(
					_elm_lang$core$Result$withDefault,
					0,
					_elm_lang$core$String$toFloat(_p27._1));
				var newRange = function () {
					var _p42 = _p27._0;
					switch (_p42) {
						case 'xmin':
							return _elm_lang$core$Native_Utils.update(
								range,
								{xmin: v});
						case 'xmax':
							return _elm_lang$core$Native_Utils.update(
								range,
								{xmax: v});
						case 'ymin':
							return _elm_lang$core$Native_Utils.update(
								range,
								{ymin: v});
						case 'ymax':
							return _elm_lang$core$Native_Utils.update(
								range,
								{ymax: v});
						default:
							return range;
					}
				}();
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Native_Utils.update(
						model,
						{pasRange: newRange}),
					_1: _elm_lang$core$Platform_Cmd$none
				};
			case 'UpdatePasScaling':
				var d = A2(
					_elm_lang$core$Maybe$withDefault,
					false,
					A2(_elm_community$list_extra$List_Extra$getAt, 2, model.pasPlotData)) ? _user$project$Main$MicFreq : _user$project$Main$MicTime;
				var max_ = _elm_lang$core$Native_Utils.eq(d, _user$project$Main$MicFreq) ? 1200 : 0;
				var maxdata = A3(
					_elm_lang$core$List$foldl,
					_user$project$Main$max3,
					{ctor: '_Tuple3', _0: 0, _1: 0, _2: 0},
					A3(_user$project$Main$getPasTimeData, model, max_, d));
				var mindata = A3(
					_elm_lang$core$List$foldl,
					_user$project$Main$min3,
					maxdata,
					A3(_user$project$Main$getPasTimeData, model, max_, d));
				var range = {
					xmin: _user$project$Main$firstElement(mindata),
					xmax: _user$project$Main$firstElement(maxdata),
					ymax: _elm_lang$core$Basics$toFloat(
						_elm_lang$core$Basics$ceiling(
							A2(
								_elm_lang$core$Basics$max,
								_user$project$Main$secondElement(maxdata),
								_user$project$Main$thirdElement(maxdata)))),
					ymin: _elm_lang$core$Basics$toFloat(
						_elm_lang$core$Basics$floor(
							A2(
								_elm_lang$core$Basics$min,
								_user$project$Main$secondElement(mindata),
								_user$project$Main$thirdElement(mindata))))
				};
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Native_Utils.update(
						model,
						{pasRange: range}),
					_1: _elm_lang$core$Platform_Cmd$none
				};
			case 'UpdateCrdRange':
				var range = model.crdRange;
				var v = A2(
					_elm_lang$core$Result$withDefault,
					0,
					_elm_lang$core$String$toFloat(_p27._1));
				var newRange = function () {
					var _p43 = _p27._0;
					switch (_p43) {
						case 'xmin':
							return _elm_lang$core$Native_Utils.update(
								range,
								{xmin: v});
						case 'xmax':
							return _elm_lang$core$Native_Utils.update(
								range,
								{xmax: v});
						case 'ymin':
							return _elm_lang$core$Native_Utils.update(
								range,
								{ymin: v});
						case 'ymax':
							return _elm_lang$core$Native_Utils.update(
								range,
								{ymax: v});
						default:
							return range;
					}
				}();
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Native_Utils.update(
						model,
						{crdRange: newRange}),
					_1: _elm_lang$core$Platform_Cmd$none
				};
			case 'UpdateCrdScaling':
				var maxdata = A3(
					_elm_lang$core$List$foldl,
					_user$project$Main$max3,
					{ctor: '_Tuple3', _0: 0, _1: 0, _2: 0},
					_user$project$Main$getRingdownData(model));
				var mindata = A3(
					_elm_lang$core$List$foldl,
					_user$project$Main$min3,
					maxdata,
					_user$project$Main$getRingdownData(model));
				var range = {
					xmin: _user$project$Main$firstElement(mindata),
					xmax: _user$project$Main$firstElement(maxdata),
					ymax: _elm_lang$core$Basics$toFloat(
						_elm_lang$core$Basics$ceiling(
							A2(
								_elm_lang$core$Basics$max,
								_user$project$Main$secondElement(maxdata),
								_user$project$Main$thirdElement(maxdata)))),
					ymin: _elm_lang$core$Basics$toFloat(
						_elm_lang$core$Basics$floor(
							A2(
								_elm_lang$core$Basics$min,
								_user$project$Main$secondElement(mindata),
								_user$project$Main$thirdElement(mindata))))
				};
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Native_Utils.update(
						model,
						{crdRange: range}),
					_1: _elm_lang$core$Platform_Cmd$none
				};
			case 'ToggleCrdPlot':
				var _p44 = _p27._0;
				var d = A2(
					_elm_lang$core$Maybe$withDefault,
					false,
					A2(_elm_community$list_extra$List_Extra$getAt, _p44, model.crdPlotData));
				var newModel = _elm_lang$core$Native_Utils.update(
					model,
					{
						crdPlotData: A3(_elm_community$list_extra$List_Extra$setAt, _p44, !d, model.crdPlotData)
					});
				return {ctor: '_Tuple2', _0: newModel, _1: _elm_lang$core$Platform_Cmd$none};
			case 'SequenceState':
				var newState = _elm_lang$core$Native_Utils.eq(model.cvt.sequence_state, 'Run') ? 'Pause' : 'Run';
				return {
					ctor: '_Tuple2',
					_0: model,
					_1: A2(_user$project$Main$changeSequenceState, newState, model)
				};
			default:
				return {
					ctor: '_Tuple2',
					_0: model,
					_1: A2(_user$project$Main$changeSequenceState, 'Reset', model)
				};
		}
	});
var _user$project$Main$viewPas = function (model) {
	return A2(
		_debois$elm_mdl$Material_Grid$grid,
		{ctor: '[]'},
		{
			ctor: '::',
			_0: A2(
				_debois$elm_mdl$Material_Grid$cell,
				{
					ctor: '::',
					_0: A2(_debois$elm_mdl$Material_Grid$size, _debois$elm_mdl$Material_Grid$Desktop, 2),
					_1: {
						ctor: '::',
						_0: A2(_debois$elm_mdl$Material_Grid$size, _debois$elm_mdl$Material_Grid$Tablet, 4),
						_1: {
							ctor: '::',
							_0: A2(_debois$elm_mdl$Material_Grid$size, _debois$elm_mdl$Material_Grid$Phone, 5),
							_1: {ctor: '[]'}
						}
					}
				},
				{
					ctor: '::',
					_0: A2(
						_debois$elm_mdl$Material_Options$div,
						{
							ctor: '::',
							_0: A2(_debois$elm_mdl$Material_Options$css, 'border-style', 'solid'),
							_1: {
								ctor: '::',
								_0: A2(_debois$elm_mdl$Material_Options$css, 'border-width', '1px'),
								_1: {
									ctor: '::',
									_0: A2(_debois$elm_mdl$Material_Options$css, 'border-radius', '5px'),
									_1: {
										ctor: '::',
										_0: A2(_debois$elm_mdl$Material_Options$css, 'padding-left', '10px'),
										_1: {
											ctor: '::',
											_0: A2(_debois$elm_mdl$Material_Options$css, 'padding-top', '10px'),
											_1: {ctor: '[]'}
										}
									}
								}
							}
						},
						{
							ctor: '::',
							_0: A3(
								_debois$elm_mdl$Material_Options$styled,
								_elm_lang$html$Html$p,
								{
									ctor: '::',
									_0: _debois$elm_mdl$Material_Typography$title,
									_1: {ctor: '[]'}
								},
								{
									ctor: '::',
									_0: _elm_lang$html$Html$text('Laser Input'),
									_1: {ctor: '[]'}
								}),
							_1: {
								ctor: '::',
								_0: A5(
									_user$project$Main$toggle,
									18,
									'Power 1',
									model.pas.cvt.enable_0,
									model,
									_user$project$Main$TogglePasLaserPower(0)),
								_1: {
									ctor: '::',
									_0: A6(
										_user$project$Main$intTextfield,
										11,
										model,
										_elm_lang$core$Maybe$Just('Channel 1'),
										model.pas.cvt.fmod_0,
										_user$project$Main$UpdateMod0,
										_elm_lang$core$Maybe$Just(
											_user$project$Main$SendModulation('0'))),
									_1: {
										ctor: '::',
										_0: A5(
											_user$project$Main$toggle,
											19,
											'Power 2',
											model.pas.cvt.enable_1,
											model,
											_user$project$Main$TogglePasLaserPower(1)),
										_1: {
											ctor: '::',
											_0: A6(
												_user$project$Main$intTextfield,
												12,
												model,
												_elm_lang$core$Maybe$Just('Channel 2'),
												model.pas.cvt.fmod_1,
												_user$project$Main$UpdateMod1,
												_elm_lang$core$Maybe$Just(
													_user$project$Main$SendModulation('1'))),
											_1: {ctor: '[]'}
										}
									}
								}
							}
						}),
					_1: {
						ctor: '::',
						_0: A2(
							_debois$elm_mdl$Material_Options$div,
							{
								ctor: '::',
								_0: A2(_debois$elm_mdl$Material_Options$css, 'border-style', 'solid'),
								_1: {
									ctor: '::',
									_0: A2(_debois$elm_mdl$Material_Options$css, 'border-width', '1px'),
									_1: {
										ctor: '::',
										_0: A2(_debois$elm_mdl$Material_Options$css, 'border-radius', '5px'),
										_1: {
											ctor: '::',
											_0: A2(_debois$elm_mdl$Material_Options$css, 'padding-left', '10px'),
											_1: {
												ctor: '::',
												_0: A2(_debois$elm_mdl$Material_Options$css, 'padding-top', '10px'),
												_1: {
													ctor: '::',
													_0: A2(_debois$elm_mdl$Material_Options$css, 'margin-top', '10px'),
													_1: {ctor: '[]'}
												}
											}
										}
									}
								}
							},
							{
								ctor: '::',
								_0: A3(
									_debois$elm_mdl$Material_Options$styled,
									_elm_lang$html$Html$p,
									{
										ctor: '::',
										_0: _debois$elm_mdl$Material_Typography$title,
										_1: {ctor: '[]'}
									},
									{
										ctor: '::',
										_0: _elm_lang$html$Html$text('Speaker Input'),
										_1: {ctor: '[]'}
									}),
								_1: {
									ctor: '::',
									_0: A5(
										_user$project$Main$toggle,
										10,
										'Channel 1',
										model.pas.cvt.speaker_0,
										model,
										_user$project$Main$ToggleSpeaker(0)),
									_1: {
										ctor: '::',
										_0: A5(
											_user$project$Main$toggle,
											11,
											'Channel 2',
											model.pas.cvt.speaker_1,
											model,
											_user$project$Main$ToggleSpeaker(1)),
										_1: {
											ctor: '::',
											_0: A6(
												_user$project$Main$txtfield,
												14,
												model,
												_elm_lang$core$Maybe$Just('Center (Hz)'),
												model.pas.cvt.spk.center,
												function (_p45) {
													return _user$project$Main$Pas(
														_user$project$Pas$UpdateSpkFcenter(_p45));
												},
												_elm_lang$core$Maybe$Just(_user$project$Main$UpdateChirp)),
											_1: {
												ctor: '::',
												_0: A6(
													_user$project$Main$txtfield,
													15,
													model,
													_elm_lang$core$Maybe$Just('Df (Hz)'),
													model.pas.cvt.spk.df,
													function (_p46) {
														return _user$project$Main$Pas(
															_user$project$Pas$UpdateSpkDf(_p46));
													},
													_elm_lang$core$Maybe$Just(_user$project$Main$UpdateChirp)),
												_1: {
													ctor: '::',
													_0: A6(
														_user$project$Main$txtfield,
														16,
														model,
														_elm_lang$core$Maybe$Just('Vscale (V)'),
														model.pas.cvt.spk.vscale,
														function (_p47) {
															return _user$project$Main$Pas(
																_user$project$Pas$UpdateSpkVscale(_p47));
														},
														_elm_lang$core$Maybe$Just(_user$project$Main$SendSpkVoltage)),
													_1: {
														ctor: '::',
														_0: A6(
															_user$project$Main$txtfield,
															17,
															model,
															_elm_lang$core$Maybe$Just('Voffset (V)'),
															model.pas.cvt.spk.voffset,
															function (_p48) {
																return _user$project$Main$Pas(
																	_user$project$Pas$UpdateSpkVoffset(_p48));
															},
															_elm_lang$core$Maybe$Just(_user$project$Main$SendSpkVoltage)),
														_1: {ctor: '[]'}
													}
												}
											}
										}
									}
								}
							}),
						_1: {ctor: '[]'}
					}
				}),
			_1: {
				ctor: '::',
				_0: A2(
					_debois$elm_mdl$Material_Grid$cell,
					{
						ctor: '::',
						_0: A2(_debois$elm_mdl$Material_Grid$size, _debois$elm_mdl$Material_Grid$Desktop, 10),
						_1: {
							ctor: '::',
							_0: A2(_debois$elm_mdl$Material_Grid$size, _debois$elm_mdl$Material_Grid$Tablet, 8),
							_1: {
								ctor: '::',
								_0: A2(_debois$elm_mdl$Material_Grid$size, _debois$elm_mdl$Material_Grid$Phone, 7),
								_1: {ctor: '[]'}
							}
						}
					},
					{
						ctor: '::',
						_0: A2(
							_debois$elm_mdl$Material_Grid$grid,
							{ctor: '[]'},
							{
								ctor: '::',
								_0: A2(
									_debois$elm_mdl$Material_Grid$cell,
									{
										ctor: '::',
										_0: A2(_debois$elm_mdl$Material_Grid$size, _debois$elm_mdl$Material_Grid$All, 12),
										_1: {ctor: '[]'}
									},
									{
										ctor: '::',
										_0: A2(
											_debois$elm_mdl$Material_Table$table,
											{ctor: '[]'},
											{
												ctor: '::',
												_0: A2(
													_debois$elm_mdl$Material_Table$thead,
													{ctor: '[]'},
													{
														ctor: '::',
														_0: A2(
															_debois$elm_mdl$Material_Table$tr,
															{ctor: '[]'},
															{
																ctor: '::',
																_0: A2(
																	_debois$elm_mdl$Material_Table$th,
																	{ctor: '[]'},
																	{
																		ctor: '::',
																		_0: _elm_lang$html$Html$text('Cell'),
																		_1: {ctor: '[]'}
																	}),
																_1: {
																	ctor: '::',
																	_0: A2(
																		_debois$elm_mdl$Material_Table$th,
																		{ctor: '[]'},
																		{
																			ctor: '::',
																			_0: A2(
																				_elm_lang$html$Html$span,
																				{
																					ctor: '::',
																					_0: A2(
																						_elm_lang$html$Html_Attributes$property,
																						'innerHTML',
																						_elm_lang$core$Json_Encode$string('f<sub>0</sub>')),
																					_1: {ctor: '[]'}
																				},
																				{ctor: '[]'}),
																			_1: {ctor: '[]'}
																		}),
																	_1: {
																		ctor: '::',
																		_0: A2(
																			_debois$elm_mdl$Material_Table$th,
																			{ctor: '[]'},
																			{
																				ctor: '::',
																				_0: _elm_lang$html$Html$text('Q'),
																				_1: {ctor: '[]'}
																			}),
																		_1: {
																			ctor: '::',
																			_0: A2(
																				_debois$elm_mdl$Material_Table$th,
																				{ctor: '[]'},
																				{
																					ctor: '::',
																					_0: _elm_lang$html$Html$text('IA'),
																					_1: {ctor: '[]'}
																				}),
																			_1: {
																				ctor: '::',
																				_0: A2(
																					_debois$elm_mdl$Material_Table$th,
																					{ctor: '[]'},
																					{
																						ctor: '::',
																						_0: A2(
																							_elm_lang$html$Html$span,
																							{
																								ctor: '::',
																								_0: A2(
																									_elm_lang$html$Html_Attributes$property,
																									'innerHTML',
																									_elm_lang$core$Json_Encode$string('&sigma;<sub>abs</sub>')),
																								_1: {ctor: '[]'}
																							},
																							{ctor: '[]'}),
																						_1: {ctor: '[]'}
																					}),
																				_1: {
																					ctor: '::',
																					_0: A2(
																						_debois$elm_mdl$Material_Table$th,
																						{ctor: '[]'},
																						{
																							ctor: '::',
																							_0: A2(
																								_elm_lang$html$Html$span,
																								{
																									ctor: '::',
																									_0: A2(
																										_elm_lang$html$Html_Attributes$property,
																										'innerHTML',
																										_elm_lang$core$Json_Encode$string('V<sub>rms</sub>')),
																									_1: {ctor: '[]'}
																								},
																								{ctor: '[]'}),
																							_1: {ctor: '[]'}
																						}),
																					_1: {ctor: '[]'}
																				}
																			}
																		}
																	}
																}
															}),
														_1: {
															ctor: '::',
															_0: A2(
																_debois$elm_mdl$Material_Table$tr,
																{ctor: '[]'},
																{
																	ctor: '::',
																	_0: A2(
																		_debois$elm_mdl$Material_Table$th,
																		{ctor: '[]'},
																		{ctor: '[]'}),
																	_1: {
																		ctor: '::',
																		_0: A2(
																			_debois$elm_mdl$Material_Table$th,
																			{ctor: '[]'},
																			{
																				ctor: '::',
																				_0: _elm_lang$html$Html$text('(Hz)'),
																				_1: {ctor: '[]'}
																			}),
																		_1: {
																			ctor: '::',
																			_0: A2(
																				_debois$elm_mdl$Material_Table$th,
																				{ctor: '[]'},
																				{
																					ctor: '::',
																					_0: _elm_lang$html$Html$text('(a.u.)'),
																					_1: {ctor: '[]'}
																				}),
																			_1: {
																				ctor: '::',
																				_0: A2(
																					_debois$elm_mdl$Material_Table$th,
																					{ctor: '[]'},
																					{
																						ctor: '::',
																						_0: _elm_lang$html$Html$text('(a.u.)'),
																						_1: {ctor: '[]'}
																					}),
																				_1: {
																					ctor: '::',
																					_0: A2(
																						_debois$elm_mdl$Material_Table$th,
																						{ctor: '[]'},
																						{
																							ctor: '::',
																							_0: A2(
																								_elm_lang$html$Html$span,
																								{
																									ctor: '::',
																									_0: A2(
																										_elm_lang$html$Html_Attributes$property,
																										'innerHTML',
																										_elm_lang$core$Json_Encode$string('(Mm<sup>-1</sup>)')),
																									_1: {ctor: '[]'}
																								},
																								{ctor: '[]'}),
																							_1: {ctor: '[]'}
																						}),
																					_1: {
																						ctor: '::',
																						_0: A2(
																							_debois$elm_mdl$Material_Table$th,
																							{ctor: '[]'},
																							{
																								ctor: '::',
																								_0: _elm_lang$html$Html$text('(V)'),
																								_1: {ctor: '[]'}
																							}),
																						_1: {ctor: '[]'}
																					}
																				}
																			}
																		}
																	}
																}),
															_1: {ctor: '[]'}
														}
													}),
												_1: {
													ctor: '::',
													_0: A2(
														_debois$elm_mdl$Material_Table$tbody,
														{ctor: '[]'},
														A2(
															_elm_lang$core$List$map,
															function (cell) {
																var data = _elm_lang$core$Tuple$second(cell);
																var name = _elm_lang$core$Tuple$first(cell);
																return A2(
																	_debois$elm_mdl$Material_Table$tr,
																	{ctor: '[]'},
																	{
																		ctor: '::',
																		_0: A2(
																			_debois$elm_mdl$Material_Table$td,
																			{ctor: '[]'},
																			{
																				ctor: '::',
																				_0: _elm_lang$html$Html$text(name),
																				_1: {ctor: '[]'}
																			}),
																		_1: {
																			ctor: '::',
																			_0: A2(
																				_debois$elm_mdl$Material_Table$td,
																				{ctor: '[]'},
																				{
																					ctor: '::',
																					_0: _elm_lang$html$Html$text(
																						_user$project$Main$printableNumeric(data.resonant_frequency)),
																					_1: {ctor: '[]'}
																				}),
																			_1: {
																				ctor: '::',
																				_0: A2(
																					_debois$elm_mdl$Material_Table$td,
																					{ctor: '[]'},
																					{
																						ctor: '::',
																						_0: _elm_lang$html$Html$text(
																							_user$project$Main$printableNumeric(data.q)),
																						_1: {ctor: '[]'}
																					}),
																				_1: {
																					ctor: '::',
																					_0: A2(
																						_debois$elm_mdl$Material_Table$td,
																						{ctor: '[]'},
																						{
																							ctor: '::',
																							_0: _elm_lang$html$Html$text(
																								_user$project$Main$printableNumeric(data.integrated_area)),
																							_1: {ctor: '[]'}
																						}),
																					_1: {
																						ctor: '::',
																						_0: A2(
																							_debois$elm_mdl$Material_Table$td,
																							{ctor: '[]'},
																							{
																								ctor: '::',
																								_0: _elm_lang$html$Html$text(
																									_user$project$Main$printableNumeric(data.absorption)),
																								_1: {ctor: '[]'}
																							}),
																						_1: {
																							ctor: '::',
																							_0: A2(
																								_debois$elm_mdl$Material_Table$td,
																								{ctor: '[]'},
																								{
																									ctor: '::',
																									_0: _elm_lang$html$Html$text(
																										_user$project$Main$printableNumeric(data.laserRMS)),
																									_1: {ctor: '[]'}
																								}),
																							_1: {ctor: '[]'}
																						}
																					}
																				}
																			}
																		}
																	});
															},
															A3(
																_elm_lang$core$List$map2,
																F2(
																	function (v0, v1) {
																		return {ctor: '_Tuple2', _0: v0, _1: v1};
																	}),
																{
																	ctor: '::',
																	_0: 'Channel 1',
																	_1: {
																		ctor: '::',
																		_0: 'Channel 2',
																		_1: {ctor: '[]'}
																	}
																},
																_elm_lang$core$Array$toList(model.pas.data.cell)))),
													_1: {ctor: '[]'}
												}
											}),
										_1: {ctor: '[]'}
									}),
								_1: {
									ctor: '::',
									_0: A2(
										_debois$elm_mdl$Material_Grid$cell,
										{
											ctor: '::',
											_0: A2(_debois$elm_mdl$Material_Grid$size, _debois$elm_mdl$Material_Grid$All, 12),
											_1: {ctor: '[]'}
										},
										{
											ctor: '::',
											_0: function () {
												var p = A2(
													_elm_lang$core$Maybe$withDefault,
													false,
													A2(_elm_community$list_extra$List_Extra$getAt, 2, model.pasPlotData)) ? _user$project$Main$MicFreq : _user$project$Main$MicTime;
												var max_ = _elm_lang$core$Native_Utils.eq(p, _user$project$Main$MicFreq) ? 1200 : 0;
												return A4(
													_user$project$Main$timeData,
													model,
													model.pasRange,
													A3(_user$project$Main$getPasTimeData, model, max_, p),
													A2(_elm_lang$core$List$take, 2, model.pasPlotData));
											}(),
											_1: {ctor: '[]'}
										}),
									_1: {
										ctor: '::',
										_0: A2(
											_debois$elm_mdl$Material_Grid$cell,
											{
												ctor: '::',
												_0: A2(_debois$elm_mdl$Material_Grid$size, _debois$elm_mdl$Material_Grid$All, 12),
												_1: {ctor: '[]'}
											},
											{
												ctor: '::',
												_0: A5(
													_debois$elm_mdl$Material_Toggles$checkbox,
													_user$project$Main$Mdl,
													{
														ctor: '::',
														_0: 100,
														_1: {ctor: '[]'}
													},
													model.mdl,
													{
														ctor: '::',
														_0: _debois$elm_mdl$Material_Toggles$value(
															A2(
																_elm_lang$core$Maybe$withDefault,
																false,
																A2(_elm_community$list_extra$List_Extra$getAt, 0, model.pasPlotData))),
														_1: {
															ctor: '::',
															_0: _debois$elm_mdl$Material_Toggles$ripple,
															_1: {
																ctor: '::',
																_0: _debois$elm_mdl$Material_Options$onToggle(
																	_user$project$Main$TogglePasPlot(0)),
																_1: {
																	ctor: '::',
																	_0: A2(_debois$elm_mdl$Material_Options$css, 'color', 'blue'),
																	_1: {ctor: '[]'}
																}
															}
														}
													},
													{
														ctor: '::',
														_0: _elm_lang$html$Html$text('Channel 1'),
														_1: {ctor: '[]'}
													}),
												_1: {
													ctor: '::',
													_0: A5(
														_debois$elm_mdl$Material_Toggles$checkbox,
														_user$project$Main$Mdl,
														{
															ctor: '::',
															_0: 101,
															_1: {ctor: '[]'}
														},
														model.mdl,
														{
															ctor: '::',
															_0: _debois$elm_mdl$Material_Toggles$value(
																A2(
																	_elm_lang$core$Maybe$withDefault,
																	false,
																	A2(_elm_community$list_extra$List_Extra$getAt, 1, model.pasPlotData))),
															_1: {
																ctor: '::',
																_0: _debois$elm_mdl$Material_Options$onToggle(
																	_user$project$Main$TogglePasPlot(1)),
																_1: {
																	ctor: '::',
																	_0: _debois$elm_mdl$Material_Toggles$ripple,
																	_1: {
																		ctor: '::',
																		_0: A2(_debois$elm_mdl$Material_Options$css, 'color', 'red'),
																		_1: {ctor: '[]'}
																	}
																}
															}
														},
														{
															ctor: '::',
															_0: _elm_lang$html$Html$text('Channel 2'),
															_1: {ctor: '[]'}
														}),
													_1: {
														ctor: '::',
														_0: A5(
															_debois$elm_mdl$Material_Toggles$switch,
															_user$project$Main$Mdl,
															{
																ctor: '::',
																_0: 102,
																_1: {ctor: '[]'}
															},
															model.mdl,
															{
																ctor: '::',
																_0: _debois$elm_mdl$Material_Toggles$value(
																	A2(
																		_elm_lang$core$Maybe$withDefault,
																		false,
																		A2(_elm_community$list_extra$List_Extra$getAt, 2, model.pasPlotData))),
																_1: {
																	ctor: '::',
																	_0: _debois$elm_mdl$Material_Options$onToggle(
																		_user$project$Main$TogglePasPlot(2)),
																	_1: {ctor: '[]'}
																}
															},
															{
																ctor: '::',
																_0: function () {
																	var s = A2(
																		_elm_lang$core$Maybe$withDefault,
																		false,
																		A2(_elm_community$list_extra$List_Extra$getAt, 2, model.pasPlotData)) ? 'Frequency' : 'Time';
																	return _elm_lang$html$Html$text(s);
																}(),
																_1: {ctor: '[]'}
															}),
														_1: {
															ctor: '::',
															_0: A5(
																_debois$elm_mdl$Material_Textfield$render,
																_user$project$Main$Mdl,
																{
																	ctor: '::',
																	_0: 103,
																	_1: {ctor: '[]'}
																},
																model.mdl,
																{
																	ctor: '::',
																	_0: _debois$elm_mdl$Material_Textfield$floatingLabel,
																	_1: {
																		ctor: '::',
																		_0: _debois$elm_mdl$Material_Textfield$value(
																			_elm_lang$core$Basics$toString(model.pasRange.xmin)),
																		_1: {
																			ctor: '::',
																			_0: _debois$elm_mdl$Material_Options$onInput(
																				_user$project$Main$UpdatePasRange('xmin')),
																			_1: {
																				ctor: '::',
																				_0: A2(_debois$elm_mdl$Material_Options$css, 'width', '125px'),
																				_1: {
																					ctor: '::',
																					_0: _debois$elm_mdl$Material_Textfield$maxlength(15),
																					_1: {
																						ctor: '::',
																						_0: _debois$elm_mdl$Material_Textfield$label('xMin'),
																						_1: {ctor: '[]'}
																					}
																				}
																			}
																		}
																	}
																},
																{ctor: '[]'}),
															_1: {
																ctor: '::',
																_0: A5(
																	_debois$elm_mdl$Material_Textfield$render,
																	_user$project$Main$Mdl,
																	{
																		ctor: '::',
																		_0: 103,
																		_1: {ctor: '[]'}
																	},
																	model.mdl,
																	{
																		ctor: '::',
																		_0: _debois$elm_mdl$Material_Textfield$floatingLabel,
																		_1: {
																			ctor: '::',
																			_0: _debois$elm_mdl$Material_Textfield$value(
																				_elm_lang$core$Basics$toString(model.pasRange.xmax)),
																			_1: {
																				ctor: '::',
																				_0: _debois$elm_mdl$Material_Options$onInput(
																					_user$project$Main$UpdatePasRange('xmax')),
																				_1: {
																					ctor: '::',
																					_0: A2(_debois$elm_mdl$Material_Options$css, 'width', '125px'),
																					_1: {
																						ctor: '::',
																						_0: _debois$elm_mdl$Material_Textfield$maxlength(15),
																						_1: {
																							ctor: '::',
																							_0: _debois$elm_mdl$Material_Textfield$label('xMax'),
																							_1: {ctor: '[]'}
																						}
																					}
																				}
																			}
																		}
																	},
																	{ctor: '[]'}),
																_1: {
																	ctor: '::',
																	_0: A5(
																		_debois$elm_mdl$Material_Textfield$render,
																		_user$project$Main$Mdl,
																		{
																			ctor: '::',
																			_0: 104,
																			_1: {ctor: '[]'}
																		},
																		model.mdl,
																		{
																			ctor: '::',
																			_0: _debois$elm_mdl$Material_Textfield$floatingLabel,
																			_1: {
																				ctor: '::',
																				_0: _debois$elm_mdl$Material_Textfield$value(
																					_elm_lang$core$Basics$toString(model.pasRange.ymin)),
																				_1: {
																					ctor: '::',
																					_0: _debois$elm_mdl$Material_Options$onInput(
																						_user$project$Main$UpdatePasRange('ymin')),
																					_1: {
																						ctor: '::',
																						_0: A2(_debois$elm_mdl$Material_Options$css, 'width', '125px'),
																						_1: {
																							ctor: '::',
																							_0: _debois$elm_mdl$Material_Textfield$maxlength(15),
																							_1: {
																								ctor: '::',
																								_0: _debois$elm_mdl$Material_Textfield$label('yMin'),
																								_1: {ctor: '[]'}
																							}
																						}
																					}
																				}
																			}
																		},
																		{ctor: '[]'}),
																	_1: {
																		ctor: '::',
																		_0: A5(
																			_debois$elm_mdl$Material_Textfield$render,
																			_user$project$Main$Mdl,
																			{
																				ctor: '::',
																				_0: 105,
																				_1: {ctor: '[]'}
																			},
																			model.mdl,
																			{
																				ctor: '::',
																				_0: _debois$elm_mdl$Material_Textfield$floatingLabel,
																				_1: {
																					ctor: '::',
																					_0: _debois$elm_mdl$Material_Textfield$value(
																						_elm_lang$core$Basics$toString(model.pasRange.ymax)),
																					_1: {
																						ctor: '::',
																						_0: _debois$elm_mdl$Material_Options$onInput(
																							_user$project$Main$UpdatePasRange('ymax')),
																						_1: {
																							ctor: '::',
																							_0: A2(_debois$elm_mdl$Material_Options$css, 'width', '125px'),
																							_1: {
																								ctor: '::',
																								_0: _debois$elm_mdl$Material_Textfield$maxlength(15),
																								_1: {
																									ctor: '::',
																									_0: _debois$elm_mdl$Material_Textfield$label('yMax'),
																									_1: {ctor: '[]'}
																								}
																							}
																						}
																					}
																				}
																			},
																			{ctor: '[]'}),
																		_1: {
																			ctor: '::',
																			_0: A4(_user$project$Main$button, 106, 'Autoscale 1x', model, _user$project$Main$UpdatePasScaling),
																			_1: {ctor: '[]'}
																		}
																	}
																}
															}
														}
													}
												}
											}),
										_1: {ctor: '[]'}
									}
								}
							}),
						_1: {ctor: '[]'}
					}),
				_1: {ctor: '[]'}
			}
		});
};
var _user$project$Main$viewBody = function (model) {
	return A2(
		_debois$elm_mdl$Material_Grid$grid,
		{
			ctor: '::',
			_0: A2(_debois$elm_mdl$Material_Options$css, 'padding-bottom', '35px'),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: A2(
				_debois$elm_mdl$Material_Grid$cell,
				{
					ctor: '::',
					_0: A2(_debois$elm_mdl$Material_Grid$size, _debois$elm_mdl$Material_Grid$All, 12),
					_1: {ctor: '[]'}
				},
				{
					ctor: '::',
					_0: function () {
						var _p49 = model.selectedTab;
						switch (_p49) {
							case 0:
								return _user$project$Main$viewMain(model);
							case 1:
								return _user$project$Main$viewPas(model);
							case 2:
								return _user$project$Main$viewCrd(model);
							case 3:
								return _user$project$Main$viewAux(model);
							case 4:
								return _user$project$Main$viewCal(model);
							case 5:
								return _user$project$Main$viewConfig(model);
							case 6:
								return _user$project$Main$viewStatus(model);
							default:
								return _elm_lang$html$Html$text('404');
						}
					}(),
					_1: {ctor: '[]'}
				}),
			_1: {ctor: '[]'}
		});
};
var _user$project$Main$view = function (model) {
	return A4(
		_debois$elm_mdl$Material_Layout$render,
		_user$project$Main$Mdl,
		model.mdl,
		{
			ctor: '::',
			_0: _debois$elm_mdl$Material_Layout$fixedHeader,
			_1: {
				ctor: '::',
				_0: _debois$elm_mdl$Material_Layout$waterfall(true),
				_1: {
					ctor: '::',
					_0: _debois$elm_mdl$Material_Layout$onSelectTab(_user$project$Main$SelectTab),
					_1: {
						ctor: '::',
						_0: _debois$elm_mdl$Material_Layout$selectedTab(model.selectedTab),
						_1: {ctor: '[]'}
					}
				}
			}
		},
		{
			header: _user$project$Main$pageHeader(model),
			drawer: _user$project$Main$viewDrawer(model),
			tabs: {
				ctor: '_Tuple2',
				_0: {
					ctor: '::',
					_0: _elm_lang$html$Html$text('Main'),
					_1: {
						ctor: '::',
						_0: _elm_lang$html$Html$text('PAS'),
						_1: {
							ctor: '::',
							_0: _elm_lang$html$Html$text('CRD'),
							_1: {
								ctor: '::',
								_0: _elm_lang$html$Html$text('Auxilary'),
								_1: {
									ctor: '::',
									_0: _elm_lang$html$Html$text('Automation'),
									_1: {
										ctor: '::',
										_0: _elm_lang$html$Html$text('Configuration'),
										_1: {
											ctor: '::',
											_0: _elm_lang$html$Html$text('Health'),
											_1: {ctor: '[]'}
										}
									}
								}
							}
						}
					}
				},
				_1: {
					ctor: '::',
					_0: _debois$elm_mdl$Material_Color$background(
						A2(_debois$elm_mdl$Material_Color$color, _debois$elm_mdl$Material_Color$LightBlue, _debois$elm_mdl$Material_Color$S400)),
					_1: {ctor: '[]'}
				}
			},
			main: {
				ctor: '::',
				_0: _user$project$Main$viewBody(model),
				_1: {
					ctor: '::',
					_0: _user$project$Main$viewFooter(model),
					_1: {ctor: '[]'}
				}
			}
		});
};
var _user$project$Main$main = _elm_lang$html$Html$program(
	{init: _user$project$Main$init, view: _user$project$Main$view, update: _user$project$Main$update, subscriptions: _user$project$Main$subscriptions})();

var Elm = {};
Elm['Main'] = Elm['Main'] || {};
if (typeof _user$project$Main$main !== 'undefined') {
    _user$project$Main$main(Elm['Main'], 'Main', undefined);
}

if (typeof define === "function" && define['amd'])
{
  define([], function() { return Elm; });
  return;
}

if (typeof module === "object")
{
  module['exports'] = Elm;
  return;
}

var globalElm = this['Elm'];
if (typeof globalElm === "undefined")
{
  this['Elm'] = Elm;
  return;
}

for (var publicModule in Elm)
{
  if (publicModule in globalElm)
  {
    throw new Error('There are two Elm modules called `' + publicModule + '` on this page! Rename one of them.');
  }
  globalElm[publicModule] = Elm[publicModule];
}

}).call(this);

