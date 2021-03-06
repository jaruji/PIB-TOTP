(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

console.warn('Compiled in DEV mode. Follow the advice at https://elm-lang.org/0.19.0/optimize for better performance and smaller assets.');


var _List_Nil_UNUSED = { $: 0 };
var _List_Nil = { $: '[]' };

function _List_Cons_UNUSED(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === elm$core$Basics$EQ ? 0 : ord === elm$core$Basics$LT ? -1 : 1;
	}));
});



// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**/
	if (x.$ === 'Set_elm_builtin')
	{
		x = elm$core$Set$toList(x);
		y = elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = elm$core$Dict$toList(x);
		y = elm$core$Dict$toList(y);
	}
	//*/

	/**_UNUSED/
	if (x.$ < 0)
	{
		x = elm$core$Dict$toList(x);
		y = elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**_UNUSED/
	if (typeof x.$ === 'undefined')
	//*/
	/**/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? elm$core$Basics$LT : n ? elm$core$Basics$GT : elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0_UNUSED = 0;
var _Utils_Tuple0 = { $: '#0' };

function _Utils_Tuple2_UNUSED(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3_UNUSED(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr_UNUSED(c) { return c; }
function _Utils_chr(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
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


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log_UNUSED = F2(function(tag, value)
{
	return value;
});

var _Debug_log = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString_UNUSED(value)
{
	return '<internals>';
}

function _Debug_toString(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
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

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash_UNUSED(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.start.line === region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'on lines ' + region.start.line + ' through ' + region.end.line;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? elm$core$Maybe$Nothing
		: elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? elm$core$Maybe$Just(n) : elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




/**/
function _Json_errorToString(error)
{
	return elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? elm$core$Result$Ok(value)
		: (value instanceof String)
			? elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return elm$core$Result$Err(A2(elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return (elm$core$Result$isOk(result)) ? result : elm$core$Result$Err(A2(elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return (elm$core$Result$isOk(result)) ? result : elm$core$Result$Err(A2(elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!elm$core$Result$isOk(result))
					{
						return elm$core$Result$Err(A2(elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return elm$core$Result$Ok(elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if (elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return elm$core$Result$Err(elm$json$Json$Decode$OneOf(elm$core$List$reverse(errors)));

		case 1:
			return elm$core$Result$Err(A2(elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!elm$core$Result$isOk(result))
		{
			return elm$core$Result$Err(A2(elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2(elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return elm$core$Result$Err(A2(elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap(value) { return { $: 0, a: value }; }
function _Json_unwrap(value) { return value.a; }

function _Json_wrap_UNUSED(value) { return value; }
function _Json_unwrap_UNUSED(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	elm$core$Result$isOk(result) || _Debug_crash(2 /**/, _Json_errorToString(result.a) /**/);
	var managers = {};
	var initPair = init(result.a);
	var model = initPair.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		var pair = A2(update, msg, model);
		stepper(model = pair.a, viewMetadata);
		_Platform_enqueueEffects(managers, pair.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, initPair.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

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


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}



var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});



// SEND REQUEST

var _Http_toTask = F3(function(router, toTask, request)
{
	return _Scheduler_binding(function(callback)
	{
		function done(response) {
			callback(toTask(request.expect.a(response)));
		}

		var xhr = new XMLHttpRequest();
		xhr.addEventListener('error', function() { done(elm$http$Http$NetworkError_); });
		xhr.addEventListener('timeout', function() { done(elm$http$Http$Timeout_); });
		xhr.addEventListener('load', function() { done(_Http_toResponse(request.expect.b, xhr)); });
		elm$core$Maybe$isJust(request.tracker) && _Http_track(router, xhr, request.tracker.a);

		try {
			xhr.open(request.method, request.url, true);
		} catch (e) {
			return done(elm$http$Http$BadUrl_(request.url));
		}

		_Http_configureRequest(xhr, request);

		request.body.a && xhr.setRequestHeader('Content-Type', request.body.a);
		xhr.send(request.body.b);

		return function() { xhr.c = true; xhr.abort(); };
	});
});


// CONFIGURE

function _Http_configureRequest(xhr, request)
{
	for (var headers = request.headers; headers.b; headers = headers.b) // WHILE_CONS
	{
		xhr.setRequestHeader(headers.a.a, headers.a.b);
	}
	xhr.timeout = request.timeout.a || 0;
	xhr.responseType = request.expect.d;
	xhr.withCredentials = request.allowCookiesFromOtherDomains;
}


// RESPONSES

function _Http_toResponse(toBody, xhr)
{
	return A2(
		200 <= xhr.status && xhr.status < 300 ? elm$http$Http$GoodStatus_ : elm$http$Http$BadStatus_,
		_Http_toMetadata(xhr),
		toBody(xhr.response)
	);
}


// METADATA

function _Http_toMetadata(xhr)
{
	return {
		url: xhr.responseURL,
		statusCode: xhr.status,
		statusText: xhr.statusText,
		headers: _Http_parseHeaders(xhr.getAllResponseHeaders())
	};
}


// HEADERS

function _Http_parseHeaders(rawHeaders)
{
	if (!rawHeaders)
	{
		return elm$core$Dict$empty;
	}

	var headers = elm$core$Dict$empty;
	var headerPairs = rawHeaders.split('\r\n');
	for (var i = headerPairs.length; i--; )
	{
		var headerPair = headerPairs[i];
		var index = headerPair.indexOf(': ');
		if (index > 0)
		{
			var key = headerPair.substring(0, index);
			var value = headerPair.substring(index + 2);

			headers = A3(elm$core$Dict$update, key, function(oldValue) {
				return elm$core$Maybe$Just(elm$core$Maybe$isJust(oldValue)
					? value + ', ' + oldValue.a
					: value
				);
			}, headers);
		}
	}
	return headers;
}


// EXPECT

var _Http_expect = F3(function(type, toBody, toValue)
{
	return {
		$: 0,
		d: type,
		b: toBody,
		a: toValue
	};
});

var _Http_mapExpect = F2(function(func, expect)
{
	return {
		$: 0,
		d: expect.d,
		b: expect.b,
		a: function(x) { return func(expect.a(x)); }
	};
});

function _Http_toDataView(arrayBuffer)
{
	return new DataView(arrayBuffer);
}


// BODY and PARTS

var _Http_emptyBody = { $: 0 };
var _Http_pair = F2(function(a, b) { return { $: 0, a: a, b: b }; });

function _Http_toFormData(parts)
{
	for (var formData = new FormData(); parts.b; parts = parts.b) // WHILE_CONS
	{
		var part = parts.a;
		formData.append(part.a, part.b);
	}
	return formData;
}

var _Http_bytesToBlob = F2(function(mime, bytes)
{
	return new Blob([bytes], { type: mime });
});


// PROGRESS

function _Http_track(router, xhr, tracker)
{
	// TODO check out lengthComputable on loadstart event

	xhr.upload.addEventListener('progress', function(event) {
		if (xhr.c) { return; }
		_Scheduler_rawSpawn(A2(elm$core$Platform$sendToSelf, router, _Utils_Tuple2(tracker, elm$http$Http$Sending({
			sent: event.loaded,
			size: event.total
		}))));
	});
	xhr.addEventListener('progress', function(event) {
		if (xhr.c) { return; }
		_Scheduler_rawSpawn(A2(elm$core$Platform$sendToSelf, router, _Utils_Tuple2(tracker, elm$http$Http$Receiving({
			received: event.loaded,
			size: event.lengthComputable ? elm$core$Maybe$Just(event.total) : elm$core$Maybe$Nothing
		}))));
	});
}



// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**_UNUSED/
	var node = args['node'];
	//*/
	/**/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS


function _VirtualDom_noScript(tag)
{
	return tag == 'script' ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return /^(on|formAction$)/i.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value)
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2(elm$json$Json$Decode$map, func, handler.a)
				:
			A3(elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		message: func(record.message),
		stopPropagation: record.stopPropagation,
		preventDefault: record.preventDefault
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.message;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.stopPropagation;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.preventDefault) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
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
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
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

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}


// CREATE

var _Regex_never = /.^/;

var _Regex_fromStringWith = F2(function(options, string)
{
	var flags = 'g';
	if (options.multiline) { flags += 'm'; }
	if (options.caseInsensitive) { flags += 'i'; }

	try
	{
		return elm$core$Maybe$Just(new RegExp(string, flags));
	}
	catch(error)
	{
		return elm$core$Maybe$Nothing;
	}
});


// USE

var _Regex_contains = F2(function(re, string)
{
	return string.match(re) !== null;
});


var _Regex_findAtMost = F3(function(n, re, str)
{
	var out = [];
	var number = 0;
	var string = str;
	var lastIndex = re.lastIndex;
	var prevLastIndex = -1;
	var result;
	while (number++ < n && (result = re.exec(string)))
	{
		if (prevLastIndex == re.lastIndex) break;
		var i = result.length - 1;
		var subs = new Array(i);
		while (i > 0)
		{
			var submatch = result[i];
			subs[--i] = submatch
				? elm$core$Maybe$Just(submatch)
				: elm$core$Maybe$Nothing;
		}
		out.push(A4(elm$regex$Regex$Match, result[0], result.index, number, _List_fromArray(subs)));
		prevLastIndex = re.lastIndex;
	}
	re.lastIndex = lastIndex;
	return _List_fromArray(out);
});


var _Regex_replaceAtMost = F4(function(n, re, replacer, string)
{
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
			submatches[--i] = submatch
				? elm$core$Maybe$Just(submatch)
				: elm$core$Maybe$Nothing;
		}
		return replacer(A4(elm$regex$Regex$Match, match, arguments[arguments.length - 2], count, _List_fromArray(submatches)));
	}
	return string.replace(re, jsReplacer);
});

var _Regex_splitAtMost = F3(function(n, re, str)
{
	var string = str;
	var out = [];
	var start = re.lastIndex;
	var restoreLastIndex = re.lastIndex;
	while (n--)
	{
		var result = re.exec(string);
		if (!result) break;
		out.push(string.slice(start, result.index));
		start = re.lastIndex;
	}
	out.push(string.slice(start));
	re.lastIndex = restoreLastIndex;
	return _List_fromArray(out);
});

var _Regex_infinity = Infinity;




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var view = impl.view;
			/**_UNUSED/
			var domNode = args['node'];
			//*/
			/**/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.setup && impl.setup(sendToApp)
			var view = impl.view;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.body);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.title) && (_VirtualDom_doc.title = title = doc.title);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.onUrlChange;
	var onUrlRequest = impl.onUrlRequest;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		setup: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.protocol === next.protocol
							&& curr.host === next.host
							&& curr.port_.a === next.port_.a
						)
							? elm$browser$Browser$Internal(next)
							: elm$browser$Browser$External(href)
					));
				}
			});
		},
		init: function(flags)
		{
			return A3(impl.init, flags, _Browser_getUrl(), key);
		},
		view: impl.view,
		update: impl.update,
		subscriptions: impl.subscriptions
	});
}

function _Browser_getUrl()
{
	return elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return elm$core$Result$isOk(result) ? elm$core$Maybe$Just(result.a) : elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { hidden: 'hidden', change: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { hidden: 'mozHidden', change: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { hidden: 'msHidden', change: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { hidden: 'webkitHidden', change: 'webkitvisibilitychange' }
		: { hidden: 'hidden', change: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail(elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		scene: _Browser_getScene(),
		viewport: {
			x: _Browser_window.pageXOffset,
			y: _Browser_window.pageYOffset,
			width: _Browser_doc.documentElement.clientWidth,
			height: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		width: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		height: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			scene: {
				width: node.scrollWidth,
				height: node.scrollHeight
			},
			viewport: {
				x: node.scrollLeft,
				y: node.scrollTop,
				width: node.clientWidth,
				height: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			scene: _Browser_getScene(),
			viewport: {
				x: x,
				y: y,
				width: _Browser_doc.documentElement.clientWidth,
				height: _Browser_doc.documentElement.clientHeight
			},
			element: {
				x: x + rect.left,
				y: y + rect.top,
				width: rect.width,
				height: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}
var author$project$Main$Model = F7(
	function (name, password, code, mode, secret, state, warning) {
		return {code: code, mode: mode, name: name, password: password, secret: secret, state: state, warning: warning};
	});
var author$project$Main$NoLogin = {$: 'NoLogin'};
var author$project$Main$None = {$: 'None'};
var author$project$Main$NotReceived = {$: 'NotReceived'};
var elm$core$Basics$False = {$: 'False'};
var elm$core$Basics$True = {$: 'True'};
var elm$core$Result$isOk = function (result) {
	if (result.$ === 'Ok') {
		return true;
	} else {
		return false;
	}
};
var elm$core$Basics$EQ = {$: 'EQ'};
var elm$core$Basics$GT = {$: 'GT'};
var elm$core$Basics$LT = {$: 'LT'};
var elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3(elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var elm$core$List$cons = _List_cons;
var elm$core$Dict$toList = function (dict) {
	return A3(
		elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var elm$core$Dict$keys = function (dict) {
	return A3(
		elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2(elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var elm$core$Set$toList = function (_n0) {
	var dict = _n0.a;
	return elm$core$Dict$keys(dict);
};
var elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var elm$core$Array$foldr = F3(
	function (func, baseCase, _n0) {
		var tree = _n0.c;
		var tail = _n0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3(elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3(elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			elm$core$Elm$JsArray$foldr,
			helper,
			A3(elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var elm$core$Array$toList = function (array) {
	return A3(elm$core$Array$foldr, elm$core$List$cons, _List_Nil, array);
};
var elm$core$Array$branchFactor = 32;
var elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 'Array_elm_builtin', a: a, b: b, c: c, d: d};
	});
var elm$core$Basics$ceiling = _Basics_ceiling;
var elm$core$Basics$fdiv = _Basics_fdiv;
var elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var elm$core$Basics$toFloat = _Basics_toFloat;
var elm$core$Array$shiftStep = elm$core$Basics$ceiling(
	A2(elm$core$Basics$logBase, 2, elm$core$Array$branchFactor));
var elm$core$Elm$JsArray$empty = _JsArray_empty;
var elm$core$Array$empty = A4(elm$core$Array$Array_elm_builtin, 0, elm$core$Array$shiftStep, elm$core$Elm$JsArray$empty, elm$core$Elm$JsArray$empty);
var elm$core$Array$Leaf = function (a) {
	return {$: 'Leaf', a: a};
};
var elm$core$Array$SubTree = function (a) {
	return {$: 'SubTree', a: a};
};
var elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var elm$core$List$reverse = function (list) {
	return A3(elm$core$List$foldl, elm$core$List$cons, _List_Nil, list);
};
var elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _n0 = A2(elm$core$Elm$JsArray$initializeFromList, elm$core$Array$branchFactor, nodes);
			var node = _n0.a;
			var remainingNodes = _n0.b;
			var newAcc = A2(
				elm$core$List$cons,
				elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var elm$core$Basics$eq = _Utils_equal;
var elm$core$Tuple$first = function (_n0) {
	var x = _n0.a;
	return x;
};
var elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = elm$core$Basics$ceiling(nodeListSize / elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2(elm$core$Elm$JsArray$initializeFromList, elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2(elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var elm$core$Basics$add = _Basics_add;
var elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var elm$core$Basics$floor = _Basics_floor;
var elm$core$Basics$gt = _Utils_gt;
var elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var elm$core$Basics$mul = _Basics_mul;
var elm$core$Basics$sub = _Basics_sub;
var elm$core$Elm$JsArray$length = _JsArray_length;
var elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.nodeListSize) {
			return A4(
				elm$core$Array$Array_elm_builtin,
				elm$core$Elm$JsArray$length(builder.tail),
				elm$core$Array$shiftStep,
				elm$core$Elm$JsArray$empty,
				builder.tail);
		} else {
			var treeLen = builder.nodeListSize * elm$core$Array$branchFactor;
			var depth = elm$core$Basics$floor(
				A2(elm$core$Basics$logBase, elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? elm$core$List$reverse(builder.nodeList) : builder.nodeList;
			var tree = A2(elm$core$Array$treeFromBuilder, correctNodeList, builder.nodeListSize);
			return A4(
				elm$core$Array$Array_elm_builtin,
				elm$core$Elm$JsArray$length(builder.tail) + treeLen,
				A2(elm$core$Basics$max, 5, depth * elm$core$Array$shiftStep),
				tree,
				builder.tail);
		}
	});
var elm$core$Basics$idiv = _Basics_idiv;
var elm$core$Basics$lt = _Utils_lt;
var elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					elm$core$Array$builderToArray,
					false,
					{nodeList: nodeList, nodeListSize: (len / elm$core$Array$branchFactor) | 0, tail: tail});
			} else {
				var leaf = elm$core$Array$Leaf(
					A3(elm$core$Elm$JsArray$initialize, elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2(elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var elm$core$Basics$le = _Utils_le;
var elm$core$Basics$remainderBy = _Basics_remainderBy;
var elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return elm$core$Array$empty;
		} else {
			var tailLen = len % elm$core$Array$branchFactor;
			var tail = A3(elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - elm$core$Array$branchFactor;
			return A5(elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var elm$core$Maybe$Just = function (a) {
	return {$: 'Just', a: a};
};
var elm$core$Maybe$Nothing = {$: 'Nothing'};
var elm$core$Result$Err = function (a) {
	return {$: 'Err', a: a};
};
var elm$core$Result$Ok = function (a) {
	return {$: 'Ok', a: a};
};
var elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 'Failure', a: a, b: b};
	});
var elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 'Index', a: a, b: b};
	});
var elm$json$Json$Decode$OneOf = function (a) {
	return {$: 'OneOf', a: a};
};
var elm$core$Basics$and = _Basics_and;
var elm$core$Basics$append = _Utils_append;
var elm$core$Basics$or = _Basics_or;
var elm$core$Char$toCode = _Char_toCode;
var elm$core$Char$isLower = function (_char) {
	var code = elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var elm$core$Char$isUpper = function (_char) {
	var code = elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var elm$core$Char$isAlpha = function (_char) {
	return elm$core$Char$isLower(_char) || elm$core$Char$isUpper(_char);
};
var elm$core$Char$isDigit = function (_char) {
	var code = elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var elm$core$Char$isAlphaNum = function (_char) {
	return elm$core$Char$isLower(_char) || (elm$core$Char$isUpper(_char) || elm$core$Char$isDigit(_char));
};
var elm$core$List$length = function (xs) {
	return A3(
		elm$core$List$foldl,
		F2(
			function (_n0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var elm$core$List$map2 = _List_map2;
var elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2(elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var elm$core$List$range = F2(
	function (lo, hi) {
		return A3(elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			elm$core$List$map2,
			f,
			A2(
				elm$core$List$range,
				0,
				elm$core$List$length(xs) - 1),
			xs);
	});
var elm$core$String$all = _String_all;
var elm$core$String$fromInt = _String_fromNumber;
var elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var elm$core$String$uncons = _String_uncons;
var elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var elm$json$Json$Decode$indent = function (str) {
	return A2(
		elm$core$String$join,
		'\n    ',
		A2(elm$core$String$split, '\n', str));
};
var elm$json$Json$Encode$encode = _Json_encode;
var elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + (elm$core$String$fromInt(i + 1) + (') ' + elm$json$Json$Decode$indent(
			elm$json$Json$Decode$errorToString(error))));
	});
var elm$json$Json$Decode$errorToString = function (error) {
	return A2(elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 'Field':
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _n1 = elm$core$String$uncons(f);
						if (_n1.$ === 'Nothing') {
							return false;
						} else {
							var _n2 = _n1.a;
							var _char = _n2.a;
							var rest = _n2.b;
							return elm$core$Char$isAlpha(_char) && A2(elm$core$String$all, elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2(elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'Index':
					var i = error.a;
					var err = error.b;
					var indexName = '[' + (elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2(elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'OneOf':
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									elm$core$String$join,
									'',
									elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										elm$core$String$join,
										'',
										elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + (elm$core$String$fromInt(
								elm$core$List$length(errors)) + ' ways:'));
							return A2(
								elm$core$String$join,
								'\n\n',
								A2(
									elm$core$List$cons,
									introduction,
									A2(elm$core$List$indexedMap, elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								elm$core$String$join,
								'',
								elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + (elm$json$Json$Decode$indent(
						A2(elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var elm$core$Platform$Cmd$batch = _Platform_batch;
var elm$core$Platform$Cmd$none = elm$core$Platform$Cmd$batch(_List_Nil);
var author$project$Main$init = function (model) {
	return _Utils_Tuple2(
		A7(author$project$Main$Model, '', '', '', author$project$Main$None, author$project$Main$NotReceived, author$project$Main$NoLogin, ''),
		elm$core$Platform$Cmd$none);
};
var elm$core$Platform$Sub$batch = _Platform_batch;
var elm$core$Platform$Sub$none = elm$core$Platform$Sub$batch(_List_Nil);
var author$project$Main$subscriptions = function (model) {
	return elm$core$Platform$Sub$none;
};
var author$project$Main$AwaitOTP = function (a) {
	return {$: 'AwaitOTP', a: a};
};
var author$project$Main$Login = function (a) {
	return {$: 'Login', a: a};
};
var author$project$Main$Received = function (a) {
	return {$: 'Received', a: a};
};
var author$project$Main$LoginResponse = function (a) {
	return {$: 'LoginResponse', a: a};
};
var elm$json$Json$Encode$object = function (pairs) {
	return _Json_wrap(
		A3(
			elm$core$List$foldl,
			F2(
				function (_n0, obj) {
					var k = _n0.a;
					var v = _n0.b;
					return A3(_Json_addField, k, v, obj);
				}),
			_Json_emptyObject(_Utils_Tuple0),
			pairs));
};
var elm$json$Json$Encode$string = _Json_wrap;
var elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var ktonon$elm_crypto$Crypto$SHA$Alg$SHA256 = {$: 'SHA256'};
var ktonon$elm_crypto$Crypto$SHA$Alg$SHA512 = {$: 'SHA512'};
var ktonon$elm_word$Word$Bit32 = {$: 'Bit32'};
var ktonon$elm_word$Word$Bit64 = {$: 'Bit64'};
var ktonon$elm_crypto$Crypto$SHA$Alg$wordSize = function (alg) {
	wordSize:
	while (true) {
		switch (alg.$) {
			case 'SHA224':
				var $temp$alg = ktonon$elm_crypto$Crypto$SHA$Alg$SHA256;
				alg = $temp$alg;
				continue wordSize;
			case 'SHA256':
				return ktonon$elm_word$Word$Bit32;
			case 'SHA384':
				var $temp$alg = ktonon$elm_crypto$Crypto$SHA$Alg$SHA512;
				alg = $temp$alg;
				continue wordSize;
			case 'SHA512':
				return ktonon$elm_word$Word$Bit64;
			case 'SHA512_224':
				var $temp$alg = ktonon$elm_crypto$Crypto$SHA$Alg$SHA512;
				alg = $temp$alg;
				continue wordSize;
			default:
				var $temp$alg = ktonon$elm_crypto$Crypto$SHA$Alg$SHA512;
				alg = $temp$alg;
				continue wordSize;
		}
	}
};
var elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							elm$core$List$foldl,
							fn,
							acc,
							elm$core$List$reverse(r4)) : A4(elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4(elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3(elm$core$List$foldr, elm$core$List$cons, ys, xs);
		}
	});
var elm$core$List$concat = function (lists) {
	return A3(elm$core$List$foldr, elm$core$List$append, _List_Nil, lists);
};
var elm$core$List$repeatHelp = F3(
	function (result, n, value) {
		repeatHelp:
		while (true) {
			if (n <= 0) {
				return result;
			} else {
				var $temp$result = A2(elm$core$List$cons, value, result),
					$temp$n = n - 1,
					$temp$value = value;
				result = $temp$result;
				n = $temp$n;
				value = $temp$value;
				continue repeatHelp;
			}
		}
	});
var elm$core$List$repeat = F2(
	function (n, value) {
		return A3(elm$core$List$repeatHelp, _List_Nil, n, value);
	});
var elm$core$Basics$modBy = _Basics_modBy;
var ktonon$elm_crypto$Crypto$SHA$Chunk$sizeInBytes = function (alg) {
	sizeInBytes:
	while (true) {
		switch (alg.$) {
			case 'SHA224':
				var $temp$alg = ktonon$elm_crypto$Crypto$SHA$Alg$SHA256;
				alg = $temp$alg;
				continue sizeInBytes;
			case 'SHA256':
				return 64;
			case 'SHA384':
				var $temp$alg = ktonon$elm_crypto$Crypto$SHA$Alg$SHA512;
				alg = $temp$alg;
				continue sizeInBytes;
			case 'SHA512':
				return 128;
			case 'SHA512_224':
				var $temp$alg = ktonon$elm_crypto$Crypto$SHA$Alg$SHA512;
				alg = $temp$alg;
				continue sizeInBytes;
			default:
				var $temp$alg = ktonon$elm_crypto$Crypto$SHA$Alg$SHA512;
				alg = $temp$alg;
				continue sizeInBytes;
		}
	}
};
var ktonon$elm_crypto$Crypto$SHA$Chunk$sizeInBits = A2(
	elm$core$Basics$composeR,
	ktonon$elm_crypto$Crypto$SHA$Chunk$sizeInBytes,
	elm$core$Basics$mul(8));
var ktonon$elm_crypto$Crypto$SHA$Preprocess$messageSizeBytes = function (alg) {
	messageSizeBytes:
	while (true) {
		switch (alg.$) {
			case 'SHA224':
				var $temp$alg = ktonon$elm_crypto$Crypto$SHA$Alg$SHA256;
				alg = $temp$alg;
				continue messageSizeBytes;
			case 'SHA256':
				return 8;
			case 'SHA384':
				var $temp$alg = ktonon$elm_crypto$Crypto$SHA$Alg$SHA512;
				alg = $temp$alg;
				continue messageSizeBytes;
			case 'SHA512':
				return 16;
			case 'SHA512_224':
				var $temp$alg = ktonon$elm_crypto$Crypto$SHA$Alg$SHA512;
				alg = $temp$alg;
				continue messageSizeBytes;
			default:
				var $temp$alg = ktonon$elm_crypto$Crypto$SHA$Alg$SHA512;
				alg = $temp$alg;
				continue messageSizeBytes;
		}
	}
};
var ktonon$elm_crypto$Crypto$SHA$Preprocess$calculateK = F2(
	function (alg, l) {
		var c = ktonon$elm_crypto$Crypto$SHA$Chunk$sizeInBits(alg);
		return A2(
			elm$core$Basics$modBy,
			c,
			((c - 1) - (8 * ktonon$elm_crypto$Crypto$SHA$Preprocess$messageSizeBytes(alg))) - A2(elm$core$Basics$modBy, c, l));
	});
var elm$core$Basics$pow = _Basics_pow;
var elm$core$Bitwise$and = _Bitwise_and;
var elm$core$Bitwise$shiftRightZfBy = _Bitwise_shiftRightZfBy;
var elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var ktonon$elm_word$Word$Bytes$fromInt = F2(
	function (byteCount, value) {
		return (byteCount > 4) ? A2(
			elm$core$List$append,
			A2(
				ktonon$elm_word$Word$Bytes$fromInt,
				byteCount - 4,
				(value / A2(elm$core$Basics$pow, 2, 32)) | 0),
			A2(ktonon$elm_word$Word$Bytes$fromInt, 4, 4294967295 & value)) : A2(
			elm$core$List$map,
			function (i) {
				return 255 & (value >>> ((byteCount - i) * A2(elm$core$Basics$pow, 2, 3)));
			},
			A2(elm$core$List$range, 1, byteCount));
	});
var ktonon$elm_crypto$Crypto$SHA$Preprocess$postfix = F2(
	function (alg, messageSize) {
		return elm$core$List$concat(
			_List_fromArray(
				[
					_List_fromArray(
					[128]),
					A2(
					elm$core$List$repeat,
					((A2(ktonon$elm_crypto$Crypto$SHA$Preprocess$calculateK, alg, messageSize) - 7) / 8) | 0,
					0),
					A2(
					ktonon$elm_word$Word$Bytes$fromInt,
					ktonon$elm_crypto$Crypto$SHA$Preprocess$messageSizeBytes(alg),
					messageSize)
				]));
	});
var ktonon$elm_crypto$Crypto$SHA$Preprocess$preprocess = F2(
	function (alg, message) {
		return A2(
			elm$core$List$append,
			message,
			A2(
				ktonon$elm_crypto$Crypto$SHA$Preprocess$postfix,
				alg,
				8 * elm$core$List$length(message)));
	});
var ktonon$elm_crypto$Crypto$SHA$Types$WorkingVars = F8(
	function (a, b, c, d, e, f, g, h) {
		return {a: a, b: b, c: c, d: d, e: e, f: f, g: g, h: h};
	});
var ktonon$elm_word$Word$D = F2(
	function (a, b) {
		return {$: 'D', a: a, b: b};
	});
var ktonon$elm_word$Word$W = function (a) {
	return {$: 'W', a: a};
};
var ktonon$elm_crypto$Crypto$SHA$Constants$initialHashValues = function (alg) {
	switch (alg.$) {
		case 'SHA224':
			return A8(
				ktonon$elm_crypto$Crypto$SHA$Types$WorkingVars,
				ktonon$elm_word$Word$W(3238371032),
				ktonon$elm_word$Word$W(914150663),
				ktonon$elm_word$Word$W(812702999),
				ktonon$elm_word$Word$W(4144912697),
				ktonon$elm_word$Word$W(4290775857),
				ktonon$elm_word$Word$W(1750603025),
				ktonon$elm_word$Word$W(1694076839),
				ktonon$elm_word$Word$W(3204075428));
		case 'SHA256':
			return A8(
				ktonon$elm_crypto$Crypto$SHA$Types$WorkingVars,
				ktonon$elm_word$Word$W(1779033703),
				ktonon$elm_word$Word$W(3144134277),
				ktonon$elm_word$Word$W(1013904242),
				ktonon$elm_word$Word$W(2773480762),
				ktonon$elm_word$Word$W(1359893119),
				ktonon$elm_word$Word$W(2600822924),
				ktonon$elm_word$Word$W(528734635),
				ktonon$elm_word$Word$W(1541459225));
		case 'SHA384':
			return A8(
				ktonon$elm_crypto$Crypto$SHA$Types$WorkingVars,
				A2(ktonon$elm_word$Word$D, 3418070365, 3238371032),
				A2(ktonon$elm_word$Word$D, 1654270250, 914150663),
				A2(ktonon$elm_word$Word$D, 2438529370, 812702999),
				A2(ktonon$elm_word$Word$D, 355462360, 4144912697),
				A2(ktonon$elm_word$Word$D, 1731405415, 4290775857),
				A2(ktonon$elm_word$Word$D, 2394180231, 1750603025),
				A2(ktonon$elm_word$Word$D, 3675008525, 1694076839),
				A2(ktonon$elm_word$Word$D, 1203062813, 3204075428));
		case 'SHA512':
			return A8(
				ktonon$elm_crypto$Crypto$SHA$Types$WorkingVars,
				A2(ktonon$elm_word$Word$D, 1779033703, 4089235720),
				A2(ktonon$elm_word$Word$D, 3144134277, 2227873595),
				A2(ktonon$elm_word$Word$D, 1013904242, 4271175723),
				A2(ktonon$elm_word$Word$D, 2773480762, 1595750129),
				A2(ktonon$elm_word$Word$D, 1359893119, 2917565137),
				A2(ktonon$elm_word$Word$D, 2600822924, 725511199),
				A2(ktonon$elm_word$Word$D, 528734635, 4215389547),
				A2(ktonon$elm_word$Word$D, 1541459225, 327033209));
		case 'SHA512_224':
			return A8(
				ktonon$elm_crypto$Crypto$SHA$Types$WorkingVars,
				A2(ktonon$elm_word$Word$D, 2352822216, 424955298),
				A2(ktonon$elm_word$Word$D, 1944164710, 2312950998),
				A2(ktonon$elm_word$Word$D, 502970286, 855612546),
				A2(ktonon$elm_word$Word$D, 1738396948, 1479516111),
				A2(ktonon$elm_word$Word$D, 258812777, 2077511080),
				A2(ktonon$elm_word$Word$D, 2011393907, 79989058),
				A2(ktonon$elm_word$Word$D, 1067287976, 1780299464),
				A2(ktonon$elm_word$Word$D, 286451373, 2446758561));
		default:
			return A8(
				ktonon$elm_crypto$Crypto$SHA$Types$WorkingVars,
				A2(ktonon$elm_word$Word$D, 573645204, 4230739756),
				A2(ktonon$elm_word$Word$D, 2673172387, 3360449730),
				A2(ktonon$elm_word$Word$D, 596883563, 1867755857),
				A2(ktonon$elm_word$Word$D, 2520282905, 1497426621),
				A2(ktonon$elm_word$Word$D, 2519219938, 2827943907),
				A2(ktonon$elm_word$Word$D, 3193839141, 1401305490),
				A2(ktonon$elm_word$Word$D, 721525244, 746961066),
				A2(ktonon$elm_word$Word$D, 246885852, 2177182882));
	}
};
var elm$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (n <= 0) {
				return list;
			} else {
				if (!list.b) {
					return list;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs;
					n = $temp$n;
					list = $temp$list;
					continue drop;
				}
			}
		}
	});
var elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var elm$core$List$takeReverse = F3(
	function (n, list, kept) {
		takeReverse:
		while (true) {
			if (n <= 0) {
				return kept;
			} else {
				if (!list.b) {
					return kept;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs,
						$temp$kept = A2(elm$core$List$cons, x, kept);
					n = $temp$n;
					list = $temp$list;
					kept = $temp$kept;
					continue takeReverse;
				}
			}
		}
	});
var elm$core$List$takeTailRec = F2(
	function (n, list) {
		return elm$core$List$reverse(
			A3(elm$core$List$takeReverse, n, list, _List_Nil));
	});
var elm$core$List$takeFast = F3(
	function (ctr, n, list) {
		if (n <= 0) {
			return _List_Nil;
		} else {
			var _n0 = _Utils_Tuple2(n, list);
			_n0$1:
			while (true) {
				_n0$5:
				while (true) {
					if (!_n0.b.b) {
						return list;
					} else {
						if (_n0.b.b.b) {
							switch (_n0.a) {
								case 1:
									break _n0$1;
								case 2:
									var _n2 = _n0.b;
									var x = _n2.a;
									var _n3 = _n2.b;
									var y = _n3.a;
									return _List_fromArray(
										[x, y]);
								case 3:
									if (_n0.b.b.b.b) {
										var _n4 = _n0.b;
										var x = _n4.a;
										var _n5 = _n4.b;
										var y = _n5.a;
										var _n6 = _n5.b;
										var z = _n6.a;
										return _List_fromArray(
											[x, y, z]);
									} else {
										break _n0$5;
									}
								default:
									if (_n0.b.b.b.b && _n0.b.b.b.b.b) {
										var _n7 = _n0.b;
										var x = _n7.a;
										var _n8 = _n7.b;
										var y = _n8.a;
										var _n9 = _n8.b;
										var z = _n9.a;
										var _n10 = _n9.b;
										var w = _n10.a;
										var tl = _n10.b;
										return (ctr > 1000) ? A2(
											elm$core$List$cons,
											x,
											A2(
												elm$core$List$cons,
												y,
												A2(
													elm$core$List$cons,
													z,
													A2(
														elm$core$List$cons,
														w,
														A2(elm$core$List$takeTailRec, n - 4, tl))))) : A2(
											elm$core$List$cons,
											x,
											A2(
												elm$core$List$cons,
												y,
												A2(
													elm$core$List$cons,
													z,
													A2(
														elm$core$List$cons,
														w,
														A3(elm$core$List$takeFast, ctr + 1, n - 4, tl)))));
									} else {
										break _n0$5;
									}
							}
						} else {
							if (_n0.a === 1) {
								break _n0$1;
							} else {
								break _n0$5;
							}
						}
					}
				}
				return list;
			}
			var _n1 = _n0.b;
			var x = _n1.a;
			return _List_fromArray(
				[x]);
		}
	});
var elm$core$List$take = F2(
	function (n, list) {
		return A3(elm$core$List$takeFast, 0, n, list);
	});
var ktonon$elm_word$Word$sizeInBytes = function (s) {
	if (s.$ === 'Bit32') {
		return 4;
	} else {
		return 8;
	}
};
var ktonon$elm_crypto$Crypto$SHA$Chunk$sizeInWords = function (alg) {
	return (ktonon$elm_crypto$Crypto$SHA$Chunk$sizeInBytes(alg) / ktonon$elm_word$Word$sizeInBytes(
		ktonon$elm_crypto$Crypto$SHA$Alg$wordSize(alg))) | 0;
};
var ktonon$elm_crypto$Crypto$SHA$Chunk$next = F2(
	function (alg, words) {
		var n = ktonon$elm_crypto$Crypto$SHA$Chunk$sizeInWords(alg);
		var chunk = A2(elm$core$List$take, n, words);
		return _Utils_Tuple2(
			elm$core$List$isEmpty(chunk) ? elm$core$Maybe$Nothing : elm$core$Maybe$Just(chunk),
			A2(elm$core$List$drop, n, words));
	});
var elm$core$Array$fromListHelp = F3(
	function (list, nodeList, nodeListSize) {
		fromListHelp:
		while (true) {
			var _n0 = A2(elm$core$Elm$JsArray$initializeFromList, elm$core$Array$branchFactor, list);
			var jsArray = _n0.a;
			var remainingItems = _n0.b;
			if (_Utils_cmp(
				elm$core$Elm$JsArray$length(jsArray),
				elm$core$Array$branchFactor) < 0) {
				return A2(
					elm$core$Array$builderToArray,
					true,
					{nodeList: nodeList, nodeListSize: nodeListSize, tail: jsArray});
			} else {
				var $temp$list = remainingItems,
					$temp$nodeList = A2(
					elm$core$List$cons,
					elm$core$Array$Leaf(jsArray),
					nodeList),
					$temp$nodeListSize = nodeListSize + 1;
				list = $temp$list;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue fromListHelp;
			}
		}
	});
var elm$core$Array$fromList = function (list) {
	if (!list.b) {
		return elm$core$Array$empty;
	} else {
		return A3(elm$core$Array$fromListHelp, list, _List_Nil, 0);
	}
};
var ktonon$elm_crypto$Crypto$SHA$Constants$roundConstants = function (alg) {
	roundConstants:
	while (true) {
		switch (alg.$) {
			case 'SHA224':
				var $temp$alg = ktonon$elm_crypto$Crypto$SHA$Alg$SHA256;
				alg = $temp$alg;
				continue roundConstants;
			case 'SHA256':
				return _List_fromArray(
					[
						ktonon$elm_word$Word$W(1116352408),
						ktonon$elm_word$Word$W(1899447441),
						ktonon$elm_word$Word$W(3049323471),
						ktonon$elm_word$Word$W(3921009573),
						ktonon$elm_word$Word$W(961987163),
						ktonon$elm_word$Word$W(1508970993),
						ktonon$elm_word$Word$W(2453635748),
						ktonon$elm_word$Word$W(2870763221),
						ktonon$elm_word$Word$W(3624381080),
						ktonon$elm_word$Word$W(310598401),
						ktonon$elm_word$Word$W(607225278),
						ktonon$elm_word$Word$W(1426881987),
						ktonon$elm_word$Word$W(1925078388),
						ktonon$elm_word$Word$W(2162078206),
						ktonon$elm_word$Word$W(2614888103),
						ktonon$elm_word$Word$W(3248222580),
						ktonon$elm_word$Word$W(3835390401),
						ktonon$elm_word$Word$W(4022224774),
						ktonon$elm_word$Word$W(264347078),
						ktonon$elm_word$Word$W(604807628),
						ktonon$elm_word$Word$W(770255983),
						ktonon$elm_word$Word$W(1249150122),
						ktonon$elm_word$Word$W(1555081692),
						ktonon$elm_word$Word$W(1996064986),
						ktonon$elm_word$Word$W(2554220882),
						ktonon$elm_word$Word$W(2821834349),
						ktonon$elm_word$Word$W(2952996808),
						ktonon$elm_word$Word$W(3210313671),
						ktonon$elm_word$Word$W(3336571891),
						ktonon$elm_word$Word$W(3584528711),
						ktonon$elm_word$Word$W(113926993),
						ktonon$elm_word$Word$W(338241895),
						ktonon$elm_word$Word$W(666307205),
						ktonon$elm_word$Word$W(773529912),
						ktonon$elm_word$Word$W(1294757372),
						ktonon$elm_word$Word$W(1396182291),
						ktonon$elm_word$Word$W(1695183700),
						ktonon$elm_word$Word$W(1986661051),
						ktonon$elm_word$Word$W(2177026350),
						ktonon$elm_word$Word$W(2456956037),
						ktonon$elm_word$Word$W(2730485921),
						ktonon$elm_word$Word$W(2820302411),
						ktonon$elm_word$Word$W(3259730800),
						ktonon$elm_word$Word$W(3345764771),
						ktonon$elm_word$Word$W(3516065817),
						ktonon$elm_word$Word$W(3600352804),
						ktonon$elm_word$Word$W(4094571909),
						ktonon$elm_word$Word$W(275423344),
						ktonon$elm_word$Word$W(430227734),
						ktonon$elm_word$Word$W(506948616),
						ktonon$elm_word$Word$W(659060556),
						ktonon$elm_word$Word$W(883997877),
						ktonon$elm_word$Word$W(958139571),
						ktonon$elm_word$Word$W(1322822218),
						ktonon$elm_word$Word$W(1537002063),
						ktonon$elm_word$Word$W(1747873779),
						ktonon$elm_word$Word$W(1955562222),
						ktonon$elm_word$Word$W(2024104815),
						ktonon$elm_word$Word$W(2227730452),
						ktonon$elm_word$Word$W(2361852424),
						ktonon$elm_word$Word$W(2428436474),
						ktonon$elm_word$Word$W(2756734187),
						ktonon$elm_word$Word$W(3204031479),
						ktonon$elm_word$Word$W(3329325298)
					]);
			case 'SHA384':
				var $temp$alg = ktonon$elm_crypto$Crypto$SHA$Alg$SHA512;
				alg = $temp$alg;
				continue roundConstants;
			case 'SHA512':
				return _List_fromArray(
					[
						A2(ktonon$elm_word$Word$D, 1116352408, 3609767458),
						A2(ktonon$elm_word$Word$D, 1899447441, 602891725),
						A2(ktonon$elm_word$Word$D, 3049323471, 3964484399),
						A2(ktonon$elm_word$Word$D, 3921009573, 2173295548),
						A2(ktonon$elm_word$Word$D, 961987163, 4081628472),
						A2(ktonon$elm_word$Word$D, 1508970993, 3053834265),
						A2(ktonon$elm_word$Word$D, 2453635748, 2937671579),
						A2(ktonon$elm_word$Word$D, 2870763221, 3664609560),
						A2(ktonon$elm_word$Word$D, 3624381080, 2734883394),
						A2(ktonon$elm_word$Word$D, 310598401, 1164996542),
						A2(ktonon$elm_word$Word$D, 607225278, 1323610764),
						A2(ktonon$elm_word$Word$D, 1426881987, 3590304994),
						A2(ktonon$elm_word$Word$D, 1925078388, 4068182383),
						A2(ktonon$elm_word$Word$D, 2162078206, 991336113),
						A2(ktonon$elm_word$Word$D, 2614888103, 633803317),
						A2(ktonon$elm_word$Word$D, 3248222580, 3479774868),
						A2(ktonon$elm_word$Word$D, 3835390401, 2666613458),
						A2(ktonon$elm_word$Word$D, 4022224774, 944711139),
						A2(ktonon$elm_word$Word$D, 264347078, 2341262773),
						A2(ktonon$elm_word$Word$D, 604807628, 2007800933),
						A2(ktonon$elm_word$Word$D, 770255983, 1495990901),
						A2(ktonon$elm_word$Word$D, 1249150122, 1856431235),
						A2(ktonon$elm_word$Word$D, 1555081692, 3175218132),
						A2(ktonon$elm_word$Word$D, 1996064986, 2198950837),
						A2(ktonon$elm_word$Word$D, 2554220882, 3999719339),
						A2(ktonon$elm_word$Word$D, 2821834349, 766784016),
						A2(ktonon$elm_word$Word$D, 2952996808, 2566594879),
						A2(ktonon$elm_word$Word$D, 3210313671, 3203337956),
						A2(ktonon$elm_word$Word$D, 3336571891, 1034457026),
						A2(ktonon$elm_word$Word$D, 3584528711, 2466948901),
						A2(ktonon$elm_word$Word$D, 113926993, 3758326383),
						A2(ktonon$elm_word$Word$D, 338241895, 168717936),
						A2(ktonon$elm_word$Word$D, 666307205, 1188179964),
						A2(ktonon$elm_word$Word$D, 773529912, 1546045734),
						A2(ktonon$elm_word$Word$D, 1294757372, 1522805485),
						A2(ktonon$elm_word$Word$D, 1396182291, 2643833823),
						A2(ktonon$elm_word$Word$D, 1695183700, 2343527390),
						A2(ktonon$elm_word$Word$D, 1986661051, 1014477480),
						A2(ktonon$elm_word$Word$D, 2177026350, 1206759142),
						A2(ktonon$elm_word$Word$D, 2456956037, 344077627),
						A2(ktonon$elm_word$Word$D, 2730485921, 1290863460),
						A2(ktonon$elm_word$Word$D, 2820302411, 3158454273),
						A2(ktonon$elm_word$Word$D, 3259730800, 3505952657),
						A2(ktonon$elm_word$Word$D, 3345764771, 106217008),
						A2(ktonon$elm_word$Word$D, 3516065817, 3606008344),
						A2(ktonon$elm_word$Word$D, 3600352804, 1432725776),
						A2(ktonon$elm_word$Word$D, 4094571909, 1467031594),
						A2(ktonon$elm_word$Word$D, 275423344, 851169720),
						A2(ktonon$elm_word$Word$D, 430227734, 3100823752),
						A2(ktonon$elm_word$Word$D, 506948616, 1363258195),
						A2(ktonon$elm_word$Word$D, 659060556, 3750685593),
						A2(ktonon$elm_word$Word$D, 883997877, 3785050280),
						A2(ktonon$elm_word$Word$D, 958139571, 3318307427),
						A2(ktonon$elm_word$Word$D, 1322822218, 3812723403),
						A2(ktonon$elm_word$Word$D, 1537002063, 2003034995),
						A2(ktonon$elm_word$Word$D, 1747873779, 3602036899),
						A2(ktonon$elm_word$Word$D, 1955562222, 1575990012),
						A2(ktonon$elm_word$Word$D, 2024104815, 1125592928),
						A2(ktonon$elm_word$Word$D, 2227730452, 2716904306),
						A2(ktonon$elm_word$Word$D, 2361852424, 442776044),
						A2(ktonon$elm_word$Word$D, 2428436474, 593698344),
						A2(ktonon$elm_word$Word$D, 2756734187, 3733110249),
						A2(ktonon$elm_word$Word$D, 3204031479, 2999351573),
						A2(ktonon$elm_word$Word$D, 3329325298, 3815920427),
						A2(ktonon$elm_word$Word$D, 3391569614, 3928383900),
						A2(ktonon$elm_word$Word$D, 3515267271, 566280711),
						A2(ktonon$elm_word$Word$D, 3940187606, 3454069534),
						A2(ktonon$elm_word$Word$D, 4118630271, 4000239992),
						A2(ktonon$elm_word$Word$D, 116418474, 1914138554),
						A2(ktonon$elm_word$Word$D, 174292421, 2731055270),
						A2(ktonon$elm_word$Word$D, 289380356, 3203993006),
						A2(ktonon$elm_word$Word$D, 460393269, 320620315),
						A2(ktonon$elm_word$Word$D, 685471733, 587496836),
						A2(ktonon$elm_word$Word$D, 852142971, 1086792851),
						A2(ktonon$elm_word$Word$D, 1017036298, 365543100),
						A2(ktonon$elm_word$Word$D, 1126000580, 2618297676),
						A2(ktonon$elm_word$Word$D, 1288033470, 3409855158),
						A2(ktonon$elm_word$Word$D, 1501505948, 4234509866),
						A2(ktonon$elm_word$Word$D, 1607167915, 987167468),
						A2(ktonon$elm_word$Word$D, 1816402316, 1246189591)
					]);
			case 'SHA512_224':
				var $temp$alg = ktonon$elm_crypto$Crypto$SHA$Alg$SHA512;
				alg = $temp$alg;
				continue roundConstants;
			default:
				var $temp$alg = ktonon$elm_crypto$Crypto$SHA$Alg$SHA512;
				alg = $temp$alg;
				continue roundConstants;
		}
	}
};
var elm$core$Elm$JsArray$appendN = _JsArray_appendN;
var elm$core$Elm$JsArray$slice = _JsArray_slice;
var elm$core$Array$appendHelpBuilder = F2(
	function (tail, builder) {
		var tailLen = elm$core$Elm$JsArray$length(tail);
		var notAppended = (elm$core$Array$branchFactor - elm$core$Elm$JsArray$length(builder.tail)) - tailLen;
		var appended = A3(elm$core$Elm$JsArray$appendN, elm$core$Array$branchFactor, builder.tail, tail);
		return (notAppended < 0) ? {
			nodeList: A2(
				elm$core$List$cons,
				elm$core$Array$Leaf(appended),
				builder.nodeList),
			nodeListSize: builder.nodeListSize + 1,
			tail: A3(elm$core$Elm$JsArray$slice, notAppended, tailLen, tail)
		} : ((!notAppended) ? {
			nodeList: A2(
				elm$core$List$cons,
				elm$core$Array$Leaf(appended),
				builder.nodeList),
			nodeListSize: builder.nodeListSize + 1,
			tail: elm$core$Elm$JsArray$empty
		} : {nodeList: builder.nodeList, nodeListSize: builder.nodeListSize, tail: appended});
	});
var elm$core$Array$bitMask = 4294967295 >>> (32 - elm$core$Array$shiftStep);
var elm$core$Basics$ge = _Utils_ge;
var elm$core$Elm$JsArray$push = _JsArray_push;
var elm$core$Elm$JsArray$singleton = _JsArray_singleton;
var elm$core$Elm$JsArray$unsafeGet = _JsArray_unsafeGet;
var elm$core$Elm$JsArray$unsafeSet = _JsArray_unsafeSet;
var elm$core$Array$insertTailInTree = F4(
	function (shift, index, tail, tree) {
		var pos = elm$core$Array$bitMask & (index >>> shift);
		if (_Utils_cmp(
			pos,
			elm$core$Elm$JsArray$length(tree)) > -1) {
			if (shift === 5) {
				return A2(
					elm$core$Elm$JsArray$push,
					elm$core$Array$Leaf(tail),
					tree);
			} else {
				var newSub = elm$core$Array$SubTree(
					A4(elm$core$Array$insertTailInTree, shift - elm$core$Array$shiftStep, index, tail, elm$core$Elm$JsArray$empty));
				return A2(elm$core$Elm$JsArray$push, newSub, tree);
			}
		} else {
			var value = A2(elm$core$Elm$JsArray$unsafeGet, pos, tree);
			if (value.$ === 'SubTree') {
				var subTree = value.a;
				var newSub = elm$core$Array$SubTree(
					A4(elm$core$Array$insertTailInTree, shift - elm$core$Array$shiftStep, index, tail, subTree));
				return A3(elm$core$Elm$JsArray$unsafeSet, pos, newSub, tree);
			} else {
				var newSub = elm$core$Array$SubTree(
					A4(
						elm$core$Array$insertTailInTree,
						shift - elm$core$Array$shiftStep,
						index,
						tail,
						elm$core$Elm$JsArray$singleton(value)));
				return A3(elm$core$Elm$JsArray$unsafeSet, pos, newSub, tree);
			}
		}
	});
var elm$core$Bitwise$shiftLeftBy = _Bitwise_shiftLeftBy;
var elm$core$Array$unsafeReplaceTail = F2(
	function (newTail, _n0) {
		var len = _n0.a;
		var startShift = _n0.b;
		var tree = _n0.c;
		var tail = _n0.d;
		var originalTailLen = elm$core$Elm$JsArray$length(tail);
		var newTailLen = elm$core$Elm$JsArray$length(newTail);
		var newArrayLen = len + (newTailLen - originalTailLen);
		if (_Utils_eq(newTailLen, elm$core$Array$branchFactor)) {
			var overflow = _Utils_cmp(newArrayLen >>> elm$core$Array$shiftStep, 1 << startShift) > 0;
			if (overflow) {
				var newShift = startShift + elm$core$Array$shiftStep;
				var newTree = A4(
					elm$core$Array$insertTailInTree,
					newShift,
					len,
					newTail,
					elm$core$Elm$JsArray$singleton(
						elm$core$Array$SubTree(tree)));
				return A4(elm$core$Array$Array_elm_builtin, newArrayLen, newShift, newTree, elm$core$Elm$JsArray$empty);
			} else {
				return A4(
					elm$core$Array$Array_elm_builtin,
					newArrayLen,
					startShift,
					A4(elm$core$Array$insertTailInTree, startShift, len, newTail, tree),
					elm$core$Elm$JsArray$empty);
			}
		} else {
			return A4(elm$core$Array$Array_elm_builtin, newArrayLen, startShift, tree, newTail);
		}
	});
var elm$core$Array$appendHelpTree = F2(
	function (toAppend, array) {
		var len = array.a;
		var tree = array.c;
		var tail = array.d;
		var itemsToAppend = elm$core$Elm$JsArray$length(toAppend);
		var notAppended = (elm$core$Array$branchFactor - elm$core$Elm$JsArray$length(tail)) - itemsToAppend;
		var appended = A3(elm$core$Elm$JsArray$appendN, elm$core$Array$branchFactor, tail, toAppend);
		var newArray = A2(elm$core$Array$unsafeReplaceTail, appended, array);
		if (notAppended < 0) {
			var nextTail = A3(elm$core$Elm$JsArray$slice, notAppended, itemsToAppend, toAppend);
			return A2(elm$core$Array$unsafeReplaceTail, nextTail, newArray);
		} else {
			return newArray;
		}
	});
var elm$core$Elm$JsArray$foldl = _JsArray_foldl;
var elm$core$Array$builderFromArray = function (_n0) {
	var len = _n0.a;
	var tree = _n0.c;
	var tail = _n0.d;
	var helper = F2(
		function (node, acc) {
			if (node.$ === 'SubTree') {
				var subTree = node.a;
				return A3(elm$core$Elm$JsArray$foldl, helper, acc, subTree);
			} else {
				return A2(elm$core$List$cons, node, acc);
			}
		});
	return {
		nodeList: A3(elm$core$Elm$JsArray$foldl, helper, _List_Nil, tree),
		nodeListSize: (len / elm$core$Array$branchFactor) | 0,
		tail: tail
	};
};
var elm$core$Array$append = F2(
	function (a, _n0) {
		var aTail = a.d;
		var bLen = _n0.a;
		var bTree = _n0.c;
		var bTail = _n0.d;
		if (_Utils_cmp(bLen, elm$core$Array$branchFactor * 4) < 1) {
			var foldHelper = F2(
				function (node, array) {
					if (node.$ === 'SubTree') {
						var tree = node.a;
						return A3(elm$core$Elm$JsArray$foldl, foldHelper, array, tree);
					} else {
						var leaf = node.a;
						return A2(elm$core$Array$appendHelpTree, leaf, array);
					}
				});
			return A2(
				elm$core$Array$appendHelpTree,
				bTail,
				A3(elm$core$Elm$JsArray$foldl, foldHelper, a, bTree));
		} else {
			var foldHelper = F2(
				function (node, builder) {
					if (node.$ === 'SubTree') {
						var tree = node.a;
						return A3(elm$core$Elm$JsArray$foldl, foldHelper, builder, tree);
					} else {
						var leaf = node.a;
						return A2(elm$core$Array$appendHelpBuilder, leaf, builder);
					}
				});
			return A2(
				elm$core$Array$builderToArray,
				true,
				A2(
					elm$core$Array$appendHelpBuilder,
					bTail,
					A3(
						elm$core$Elm$JsArray$foldl,
						foldHelper,
						elm$core$Array$builderFromArray(a),
						bTree)));
		}
	});
var elm$core$Array$getHelp = F3(
	function (shift, index, tree) {
		getHelp:
		while (true) {
			var pos = elm$core$Array$bitMask & (index >>> shift);
			var _n0 = A2(elm$core$Elm$JsArray$unsafeGet, pos, tree);
			if (_n0.$ === 'SubTree') {
				var subTree = _n0.a;
				var $temp$shift = shift - elm$core$Array$shiftStep,
					$temp$index = index,
					$temp$tree = subTree;
				shift = $temp$shift;
				index = $temp$index;
				tree = $temp$tree;
				continue getHelp;
			} else {
				var values = _n0.a;
				return A2(elm$core$Elm$JsArray$unsafeGet, elm$core$Array$bitMask & index, values);
			}
		}
	});
var elm$core$Array$tailIndex = function (len) {
	return (len >>> 5) << 5;
};
var elm$core$Array$get = F2(
	function (index, _n0) {
		var len = _n0.a;
		var startShift = _n0.b;
		var tree = _n0.c;
		var tail = _n0.d;
		return ((index < 0) || (_Utils_cmp(index, len) > -1)) ? elm$core$Maybe$Nothing : ((_Utils_cmp(
			index,
			elm$core$Array$tailIndex(len)) > -1) ? elm$core$Maybe$Just(
			A2(elm$core$Elm$JsArray$unsafeGet, elm$core$Array$bitMask & index, tail)) : elm$core$Maybe$Just(
			A3(elm$core$Array$getHelp, startShift, index, tree)));
	});
var elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var ktonon$elm_word$Word$Mismatch = {$: 'Mismatch'};
var ktonon$elm_crypto$Crypto$SHA$MessageSchedule$at = function (i) {
	return A2(
		elm$core$Basics$composeR,
		elm$core$Array$get(i),
		elm$core$Maybe$withDefault(ktonon$elm_word$Word$Mismatch));
};
var ktonon$elm_word$Word$Helpers$lowMask = function (n) {
	switch (n) {
		case 0:
			return 0;
		case 1:
			return 1;
		case 2:
			return 3;
		case 3:
			return 7;
		case 4:
			return 15;
		case 5:
			return 31;
		case 6:
			return 63;
		case 7:
			return 127;
		case 8:
			return 255;
		case 9:
			return 511;
		case 10:
			return 1023;
		case 11:
			return 2047;
		case 12:
			return 4095;
		case 13:
			return 8191;
		case 14:
			return 16383;
		case 15:
			return 32767;
		case 16:
			return 65535;
		case 17:
			return 131071;
		case 18:
			return 262143;
		case 19:
			return 524287;
		case 20:
			return 1048575;
		case 21:
			return 2097151;
		case 22:
			return 4194303;
		case 23:
			return 8388607;
		case 24:
			return 16777215;
		case 25:
			return 33554431;
		case 26:
			return 67108863;
		case 27:
			return 134217727;
		case 28:
			return 268435455;
		case 29:
			return 536870911;
		case 30:
			return 1073741823;
		case 31:
			return 2147483647;
		default:
			return 4294967295;
	}
};
var ktonon$elm_word$Word$Helpers$safeShiftRightZfBy = F2(
	function (n, val) {
		return (n >= 32) ? 0 : (val >>> n);
	});
var ktonon$elm_word$Word$dShiftRightZfBy = F2(
	function (n, _n0) {
		var xh = _n0.a;
		var xl = _n0.b;
		return (n > 32) ? _Utils_Tuple2(
			0,
			A2(ktonon$elm_word$Word$Helpers$safeShiftRightZfBy, n - 32, xh)) : _Utils_Tuple2(
			A2(ktonon$elm_word$Word$Helpers$safeShiftRightZfBy, n, xh),
			A2(ktonon$elm_word$Word$Helpers$safeShiftRightZfBy, n, xl) + ((ktonon$elm_word$Word$Helpers$lowMask(n) & xh) << (32 - n)));
	});
var ktonon$elm_word$Word$Helpers$rotatedLowBits = F2(
	function (n, val) {
		return elm$core$Basics$add(
			(ktonon$elm_word$Word$Helpers$lowMask(n) & val) << (32 - n));
	});
var ktonon$elm_word$Word$rotateRightBy = F2(
	function (unboundN, word) {
		switch (word.$) {
			case 'W':
				var x = word.a;
				var n = A2(elm$core$Basics$modBy, 32, unboundN);
				return ktonon$elm_word$Word$W(
					A3(
						ktonon$elm_word$Word$Helpers$rotatedLowBits,
						n,
						x,
						A2(ktonon$elm_word$Word$Helpers$safeShiftRightZfBy, n, x)));
			case 'D':
				var xh = word.a;
				var xl = word.b;
				var n = A2(elm$core$Basics$modBy, 64, unboundN);
				if (n > 32) {
					var n_ = n - 32;
					var _n1 = A2(
						ktonon$elm_word$Word$dShiftRightZfBy,
						n_,
						_Utils_Tuple2(xl, xh));
					var zh = _n1.a;
					var zl = _n1.b;
					return A2(
						ktonon$elm_word$Word$D,
						A3(ktonon$elm_word$Word$Helpers$rotatedLowBits, n_, xh, zh),
						zl);
				} else {
					var _n2 = A2(
						ktonon$elm_word$Word$dShiftRightZfBy,
						n,
						_Utils_Tuple2(xh, xl));
					var zh = _n2.a;
					var zl = _n2.b;
					return A2(
						ktonon$elm_word$Word$D,
						A3(ktonon$elm_word$Word$Helpers$rotatedLowBits, n, xl, zh),
						zl);
				}
			default:
				return ktonon$elm_word$Word$Mismatch;
		}
	});
var ktonon$elm_word$Word$shiftRightZfBy = F2(
	function (n, word) {
		switch (word.$) {
			case 'W':
				var x = word.a;
				return ktonon$elm_word$Word$W(
					A2(ktonon$elm_word$Word$Helpers$safeShiftRightZfBy, n, x));
			case 'D':
				var xh = word.a;
				var xl = word.b;
				var _n1 = A2(
					ktonon$elm_word$Word$dShiftRightZfBy,
					n,
					_Utils_Tuple2(xh, xl));
				var zh = _n1.a;
				var zl = _n1.b;
				return A2(ktonon$elm_word$Word$D, zh, zl);
			default:
				return ktonon$elm_word$Word$Mismatch;
		}
	});
var elm$core$Bitwise$xor = _Bitwise_xor;
var ktonon$elm_word$Word$xor = F2(
	function (wx, wy) {
		var _n0 = _Utils_Tuple2(wx, wy);
		_n0$2:
		while (true) {
			switch (_n0.a.$) {
				case 'W':
					if (_n0.b.$ === 'W') {
						var x = _n0.a.a;
						var y = _n0.b.a;
						return ktonon$elm_word$Word$W(x ^ y);
					} else {
						break _n0$2;
					}
				case 'D':
					if (_n0.b.$ === 'D') {
						var _n1 = _n0.a;
						var xh = _n1.a;
						var xl = _n1.b;
						var _n2 = _n0.b;
						var yh = _n2.a;
						var yl = _n2.b;
						return A2(ktonon$elm_word$Word$D, xh ^ yh, xl ^ yl);
					} else {
						break _n0$2;
					}
				default:
					break _n0$2;
			}
		}
		return ktonon$elm_word$Word$Mismatch;
	});
var ktonon$elm_crypto$Crypto$SHA$MessageSchedule$sigma0 = F2(
	function (alg, word) {
		sigma0:
		while (true) {
			switch (alg.$) {
				case 'SHA224':
					var $temp$alg = ktonon$elm_crypto$Crypto$SHA$Alg$SHA256,
						$temp$word = word;
					alg = $temp$alg;
					word = $temp$word;
					continue sigma0;
				case 'SHA384':
					var $temp$alg = ktonon$elm_crypto$Crypto$SHA$Alg$SHA512,
						$temp$word = word;
					alg = $temp$alg;
					word = $temp$word;
					continue sigma0;
				case 'SHA256':
					return A2(
						ktonon$elm_word$Word$xor,
						A2(ktonon$elm_word$Word$shiftRightZfBy, 3, word),
						A2(
							ktonon$elm_word$Word$xor,
							A2(ktonon$elm_word$Word$rotateRightBy, 18, word),
							A2(ktonon$elm_word$Word$rotateRightBy, 7, word)));
				case 'SHA512':
					return A2(
						ktonon$elm_word$Word$xor,
						A2(ktonon$elm_word$Word$shiftRightZfBy, 7, word),
						A2(
							ktonon$elm_word$Word$xor,
							A2(ktonon$elm_word$Word$rotateRightBy, 8, word),
							A2(ktonon$elm_word$Word$rotateRightBy, 1, word)));
				case 'SHA512_224':
					var $temp$alg = ktonon$elm_crypto$Crypto$SHA$Alg$SHA512,
						$temp$word = word;
					alg = $temp$alg;
					word = $temp$word;
					continue sigma0;
				default:
					var $temp$alg = ktonon$elm_crypto$Crypto$SHA$Alg$SHA512,
						$temp$word = word;
					alg = $temp$alg;
					word = $temp$word;
					continue sigma0;
			}
		}
	});
var ktonon$elm_crypto$Crypto$SHA$MessageSchedule$sigma1 = F2(
	function (alg, word) {
		sigma1:
		while (true) {
			switch (alg.$) {
				case 'SHA224':
					var $temp$alg = ktonon$elm_crypto$Crypto$SHA$Alg$SHA256,
						$temp$word = word;
					alg = $temp$alg;
					word = $temp$word;
					continue sigma1;
				case 'SHA384':
					var $temp$alg = ktonon$elm_crypto$Crypto$SHA$Alg$SHA512,
						$temp$word = word;
					alg = $temp$alg;
					word = $temp$word;
					continue sigma1;
				case 'SHA256':
					return A2(
						ktonon$elm_word$Word$xor,
						A2(ktonon$elm_word$Word$shiftRightZfBy, 10, word),
						A2(
							ktonon$elm_word$Word$xor,
							A2(ktonon$elm_word$Word$rotateRightBy, 19, word),
							A2(ktonon$elm_word$Word$rotateRightBy, 17, word)));
				case 'SHA512':
					return A2(
						ktonon$elm_word$Word$xor,
						A2(ktonon$elm_word$Word$shiftRightZfBy, 6, word),
						A2(
							ktonon$elm_word$Word$xor,
							A2(ktonon$elm_word$Word$rotateRightBy, 61, word),
							A2(ktonon$elm_word$Word$rotateRightBy, 19, word)));
				case 'SHA512_224':
					var $temp$alg = ktonon$elm_crypto$Crypto$SHA$Alg$SHA512,
						$temp$word = word;
					alg = $temp$alg;
					word = $temp$word;
					continue sigma1;
				default:
					var $temp$alg = ktonon$elm_crypto$Crypto$SHA$Alg$SHA512,
						$temp$word = word;
					alg = $temp$alg;
					word = $temp$word;
					continue sigma1;
			}
		}
	});
var ktonon$elm_word$Word$low31mask = 2147483647;
var ktonon$elm_word$Word$carry32 = F2(
	function (x, y) {
		var _n0 = (x >>> 31) + (y >>> 31);
		switch (_n0) {
			case 0:
				return 0;
			case 2:
				return 1;
			default:
				return (1 === (((ktonon$elm_word$Word$low31mask & x) + (ktonon$elm_word$Word$low31mask & y)) >>> 31)) ? 1 : 0;
		}
	});
var ktonon$elm_word$Word$mod32 = function (val) {
	return A2(
		elm$core$Basics$modBy,
		A2(elm$core$Basics$pow, 2, 32),
		val);
};
var ktonon$elm_word$Word$add = F2(
	function (wx, wy) {
		var _n0 = _Utils_Tuple2(wx, wy);
		_n0$2:
		while (true) {
			switch (_n0.a.$) {
				case 'W':
					if (_n0.b.$ === 'W') {
						var x = _n0.a.a;
						var y = _n0.b.a;
						return ktonon$elm_word$Word$W(
							ktonon$elm_word$Word$mod32(x + y));
					} else {
						break _n0$2;
					}
				case 'D':
					if (_n0.b.$ === 'D') {
						var _n1 = _n0.a;
						var xh = _n1.a;
						var xl = _n1.b;
						var _n2 = _n0.b;
						var yh = _n2.a;
						var yl = _n2.b;
						var zl = xl + yl;
						var zh = (xh + yh) + A2(ktonon$elm_word$Word$carry32, xl, yl);
						return A2(
							ktonon$elm_word$Word$D,
							ktonon$elm_word$Word$mod32(zh),
							ktonon$elm_word$Word$mod32(zl));
					} else {
						break _n0$2;
					}
				default:
					break _n0$2;
			}
		}
		return ktonon$elm_word$Word$Mismatch;
	});
var ktonon$elm_crypto$Crypto$SHA$MessageSchedule$nextPart = F3(
	function (alg, i, w) {
		var i2 = A2(ktonon$elm_crypto$Crypto$SHA$MessageSchedule$at, i - 2, w);
		var s1 = A2(ktonon$elm_crypto$Crypto$SHA$MessageSchedule$sigma1, alg, i2);
		var i15 = A2(ktonon$elm_crypto$Crypto$SHA$MessageSchedule$at, i - 15, w);
		var s0 = A2(ktonon$elm_crypto$Crypto$SHA$MessageSchedule$sigma0, alg, i15);
		return A2(
			elm$core$Array$append,
			w,
			elm$core$Array$fromList(
				_List_fromArray(
					[
						A2(
						ktonon$elm_word$Word$add,
						s1,
						A2(
							ktonon$elm_word$Word$add,
							A2(ktonon$elm_crypto$Crypto$SHA$MessageSchedule$at, i - 7, w),
							A2(
								ktonon$elm_word$Word$add,
								s0,
								A2(ktonon$elm_crypto$Crypto$SHA$MessageSchedule$at, i - 16, w))))
					])));
	});
var ktonon$elm_crypto$Crypto$SHA$MessageSchedule$fromChunk = F2(
	function (alg, chunk) {
		var n = elm$core$List$length(
			ktonon$elm_crypto$Crypto$SHA$Constants$roundConstants(alg));
		return A3(
			elm$core$List$foldl,
			ktonon$elm_crypto$Crypto$SHA$MessageSchedule$nextPart(alg),
			elm$core$Array$fromList(chunk),
			A2(elm$core$List$range, 16, n - 1));
	});
var ktonon$elm_crypto$Crypto$SHA$Process$sum0 = F2(
	function (alg, word) {
		sum0:
		while (true) {
			switch (alg.$) {
				case 'SHA224':
					var $temp$alg = ktonon$elm_crypto$Crypto$SHA$Alg$SHA256,
						$temp$word = word;
					alg = $temp$alg;
					word = $temp$word;
					continue sum0;
				case 'SHA384':
					var $temp$alg = ktonon$elm_crypto$Crypto$SHA$Alg$SHA512,
						$temp$word = word;
					alg = $temp$alg;
					word = $temp$word;
					continue sum0;
				case 'SHA256':
					return A2(
						ktonon$elm_word$Word$xor,
						A2(ktonon$elm_word$Word$rotateRightBy, 22, word),
						A2(
							ktonon$elm_word$Word$xor,
							A2(ktonon$elm_word$Word$rotateRightBy, 13, word),
							A2(ktonon$elm_word$Word$rotateRightBy, 2, word)));
				case 'SHA512':
					return A2(
						ktonon$elm_word$Word$xor,
						A2(ktonon$elm_word$Word$rotateRightBy, 39, word),
						A2(
							ktonon$elm_word$Word$xor,
							A2(ktonon$elm_word$Word$rotateRightBy, 34, word),
							A2(ktonon$elm_word$Word$rotateRightBy, 28, word)));
				case 'SHA512_224':
					var $temp$alg = ktonon$elm_crypto$Crypto$SHA$Alg$SHA512,
						$temp$word = word;
					alg = $temp$alg;
					word = $temp$word;
					continue sum0;
				default:
					var $temp$alg = ktonon$elm_crypto$Crypto$SHA$Alg$SHA512,
						$temp$word = word;
					alg = $temp$alg;
					word = $temp$word;
					continue sum0;
			}
		}
	});
var ktonon$elm_crypto$Crypto$SHA$Process$sum1 = F2(
	function (alg, word) {
		sum1:
		while (true) {
			switch (alg.$) {
				case 'SHA224':
					var $temp$alg = ktonon$elm_crypto$Crypto$SHA$Alg$SHA256,
						$temp$word = word;
					alg = $temp$alg;
					word = $temp$word;
					continue sum1;
				case 'SHA384':
					var $temp$alg = ktonon$elm_crypto$Crypto$SHA$Alg$SHA512,
						$temp$word = word;
					alg = $temp$alg;
					word = $temp$word;
					continue sum1;
				case 'SHA256':
					return A2(
						ktonon$elm_word$Word$xor,
						A2(ktonon$elm_word$Word$rotateRightBy, 25, word),
						A2(
							ktonon$elm_word$Word$xor,
							A2(ktonon$elm_word$Word$rotateRightBy, 11, word),
							A2(ktonon$elm_word$Word$rotateRightBy, 6, word)));
				case 'SHA512':
					return A2(
						ktonon$elm_word$Word$xor,
						A2(ktonon$elm_word$Word$rotateRightBy, 41, word),
						A2(
							ktonon$elm_word$Word$xor,
							A2(ktonon$elm_word$Word$rotateRightBy, 18, word),
							A2(ktonon$elm_word$Word$rotateRightBy, 14, word)));
				case 'SHA512_224':
					var $temp$alg = ktonon$elm_crypto$Crypto$SHA$Alg$SHA512,
						$temp$word = word;
					alg = $temp$alg;
					word = $temp$word;
					continue sum1;
				default:
					var $temp$alg = ktonon$elm_crypto$Crypto$SHA$Alg$SHA512,
						$temp$word = word;
					alg = $temp$alg;
					word = $temp$word;
					continue sum1;
			}
		}
	});
var ktonon$elm_word$Word$and = F2(
	function (wx, wy) {
		var _n0 = _Utils_Tuple2(wx, wy);
		_n0$2:
		while (true) {
			switch (_n0.a.$) {
				case 'W':
					if (_n0.b.$ === 'W') {
						var x = _n0.a.a;
						var y = _n0.b.a;
						return ktonon$elm_word$Word$W(x & y);
					} else {
						break _n0$2;
					}
				case 'D':
					if (_n0.b.$ === 'D') {
						var _n1 = _n0.a;
						var xh = _n1.a;
						var xl = _n1.b;
						var _n2 = _n0.b;
						var yh = _n2.a;
						var yl = _n2.b;
						return A2(ktonon$elm_word$Word$D, xh & yh, xl & yl);
					} else {
						break _n0$2;
					}
				default:
					break _n0$2;
			}
		}
		return ktonon$elm_word$Word$Mismatch;
	});
var elm$core$Bitwise$complement = _Bitwise_complement;
var ktonon$elm_word$Word$complement = function (word) {
	switch (word.$) {
		case 'W':
			var x = word.a;
			return ktonon$elm_word$Word$W(~x);
		case 'D':
			var xh = word.a;
			var xl = word.b;
			return A2(ktonon$elm_word$Word$D, ~xh, ~xl);
		default:
			return ktonon$elm_word$Word$Mismatch;
	}
};
var ktonon$elm_crypto$Crypto$SHA$Process$compress = F3(
	function (alg, _n0, _n1) {
		var k = _n0.a;
		var w = _n0.b;
		var a = _n1.a;
		var b = _n1.b;
		var c = _n1.c;
		var d = _n1.d;
		var e = _n1.e;
		var f = _n1.f;
		var g = _n1.g;
		var h = _n1.h;
		var s1 = A2(ktonon$elm_crypto$Crypto$SHA$Process$sum1, alg, e);
		var s0 = A2(ktonon$elm_crypto$Crypto$SHA$Process$sum0, alg, a);
		var maj = A2(
			ktonon$elm_word$Word$xor,
			A2(ktonon$elm_word$Word$and, b, c),
			A2(
				ktonon$elm_word$Word$xor,
				A2(ktonon$elm_word$Word$and, a, c),
				A2(ktonon$elm_word$Word$and, a, b)));
		var temp2 = A2(ktonon$elm_word$Word$add, s0, maj);
		var ch = A2(
			ktonon$elm_word$Word$xor,
			A2(
				ktonon$elm_word$Word$and,
				g,
				ktonon$elm_word$Word$complement(e)),
			A2(ktonon$elm_word$Word$and, e, f));
		var temp1 = A2(
			ktonon$elm_word$Word$add,
			w,
			A2(
				ktonon$elm_word$Word$add,
				k,
				A2(
					ktonon$elm_word$Word$add,
					ch,
					A2(ktonon$elm_word$Word$add, s1, h))));
		return A8(
			ktonon$elm_crypto$Crypto$SHA$Types$WorkingVars,
			A2(ktonon$elm_word$Word$add, temp1, temp2),
			a,
			b,
			c,
			A2(ktonon$elm_word$Word$add, d, temp1),
			e,
			f,
			g);
	});
var ktonon$elm_crypto$Crypto$SHA$Process$compressLoop = F3(
	function (alg, workingVars, messageSchedule) {
		return A3(
			elm$core$List$foldl,
			ktonon$elm_crypto$Crypto$SHA$Process$compress(alg),
			workingVars,
			A3(
				elm$core$List$map2,
				F2(
					function (a, b) {
						return _Utils_Tuple2(a, b);
					}),
				ktonon$elm_crypto$Crypto$SHA$Constants$roundConstants(alg),
				elm$core$Array$toList(messageSchedule)));
	});
var ktonon$elm_crypto$Crypto$SHA$Types$addWorkingVars = F2(
	function (x, y) {
		return A8(
			ktonon$elm_crypto$Crypto$SHA$Types$WorkingVars,
			A2(ktonon$elm_word$Word$add, x.a, y.a),
			A2(ktonon$elm_word$Word$add, x.b, y.b),
			A2(ktonon$elm_word$Word$add, x.c, y.c),
			A2(ktonon$elm_word$Word$add, x.d, y.d),
			A2(ktonon$elm_word$Word$add, x.e, y.e),
			A2(ktonon$elm_word$Word$add, x.f, y.f),
			A2(ktonon$elm_word$Word$add, x.g, y.g),
			A2(ktonon$elm_word$Word$add, x.h, y.h));
	});
var ktonon$elm_crypto$Crypto$SHA$Process$chunks_ = F3(
	function (alg, words, currentHash) {
		chunks_:
		while (true) {
			var _n0 = A2(ktonon$elm_crypto$Crypto$SHA$Chunk$next, alg, words);
			if (_n0.a.$ === 'Nothing') {
				var _n1 = _n0.a;
				return currentHash;
			} else {
				var chunk = _n0.a.a;
				var rest = _n0.b;
				var vars = A2(
					ktonon$elm_crypto$Crypto$SHA$Types$addWorkingVars,
					currentHash,
					A3(
						ktonon$elm_crypto$Crypto$SHA$Process$compressLoop,
						alg,
						currentHash,
						A2(ktonon$elm_crypto$Crypto$SHA$MessageSchedule$fromChunk, alg, chunk)));
				var $temp$alg = alg,
					$temp$words = rest,
					$temp$currentHash = vars;
				alg = $temp$alg;
				words = $temp$words;
				currentHash = $temp$currentHash;
				continue chunks_;
			}
		}
	});
var elm$core$List$concatMap = F2(
	function (f, list) {
		return elm$core$List$concat(
			A2(elm$core$List$map, f, list));
	});
var ktonon$elm_crypto$Crypto$SHA$Types$toSingleWord = function (word) {
	if (word.$ === 'D') {
		var xh = word.a;
		var xl = word.b;
		return _List_fromArray(
			[
				ktonon$elm_word$Word$W(xh),
				ktonon$elm_word$Word$W(xl)
			]);
	} else {
		return _List_fromArray(
			[word]);
	}
};
var ktonon$elm_crypto$Crypto$SHA$Types$workingVarsToWords = F2(
	function (alg, _n0) {
		var a = _n0.a;
		var b = _n0.b;
		var c = _n0.c;
		var d = _n0.d;
		var e = _n0.e;
		var f = _n0.f;
		var g = _n0.g;
		var h = _n0.h;
		switch (alg.$) {
			case 'SHA224':
				return elm$core$Array$fromList(
					_List_fromArray(
						[a, b, c, d, e, f, g]));
			case 'SHA256':
				return elm$core$Array$fromList(
					_List_fromArray(
						[a, b, c, d, e, f, g, h]));
			case 'SHA384':
				return elm$core$Array$fromList(
					_List_fromArray(
						[a, b, c, d, e, f]));
			case 'SHA512':
				return elm$core$Array$fromList(
					_List_fromArray(
						[a, b, c, d, e, f, g, h]));
			case 'SHA512_224':
				return elm$core$Array$fromList(
					A2(
						elm$core$List$take,
						7,
						A2(
							elm$core$List$concatMap,
							ktonon$elm_crypto$Crypto$SHA$Types$toSingleWord,
							_List_fromArray(
								[a, b, c, d]))));
			default:
				return elm$core$Array$fromList(
					_List_fromArray(
						[a, b, c, d]));
		}
	});
var ktonon$elm_crypto$Crypto$SHA$Process$chunks = F2(
	function (alg, words) {
		return A2(
			ktonon$elm_crypto$Crypto$SHA$Types$workingVarsToWords,
			alg,
			A3(
				ktonon$elm_crypto$Crypto$SHA$Process$chunks_,
				alg,
				elm$core$Array$toList(words),
				ktonon$elm_crypto$Crypto$SHA$Constants$initialHashValues(alg)));
	});
var elm$core$Array$push = F2(
	function (a, array) {
		var tail = array.d;
		return A2(
			elm$core$Array$unsafeReplaceTail,
			A2(elm$core$Elm$JsArray$push, a, tail),
			array);
	});
var ktonon$elm_word$Word$FourBytes = F4(
	function (a, b, c, d) {
		return {$: 'FourBytes', a: a, b: b, c: c, d: d};
	});
var ktonon$elm_word$Word$int32FromBytes = function (_n0) {
	var x3 = _n0.a;
	var x2 = _n0.b;
	var x1 = _n0.c;
	var x0 = _n0.d;
	return ((x0 + (x1 * A2(elm$core$Basics$pow, 2, 8))) + (x2 * A2(elm$core$Basics$pow, 2, 16))) + (x3 * A2(elm$core$Basics$pow, 2, 24));
};
var ktonon$elm_word$Word$pad4 = function (bytes) {
	_n0$4:
	while (true) {
		if (bytes.b) {
			if (bytes.b.b) {
				if (bytes.b.b.b) {
					if (bytes.b.b.b.b) {
						if (!bytes.b.b.b.b.b) {
							var x3 = bytes.a;
							var _n1 = bytes.b;
							var x2 = _n1.a;
							var _n2 = _n1.b;
							var x1 = _n2.a;
							var _n3 = _n2.b;
							var x0 = _n3.a;
							return A4(ktonon$elm_word$Word$FourBytes, x3, x2, x1, x0);
						} else {
							break _n0$4;
						}
					} else {
						var x3 = bytes.a;
						var _n4 = bytes.b;
						var x2 = _n4.a;
						var _n5 = _n4.b;
						var x1 = _n5.a;
						return A4(ktonon$elm_word$Word$FourBytes, x3, x2, x1, 0);
					}
				} else {
					var x3 = bytes.a;
					var _n6 = bytes.b;
					var x2 = _n6.a;
					return A4(ktonon$elm_word$Word$FourBytes, x3, x2, 0, 0);
				}
			} else {
				var x3 = bytes.a;
				return A4(ktonon$elm_word$Word$FourBytes, x3, 0, 0, 0);
			}
		} else {
			break _n0$4;
		}
	}
	return A4(ktonon$elm_word$Word$FourBytes, 0, 0, 0, 0);
};
var ktonon$elm_word$Word$accWords = F3(
	function (wordSize, bytes, acc) {
		accWords:
		while (true) {
			var _n0 = _Utils_Tuple2(wordSize, bytes);
			_n0$2:
			while (true) {
				if (_n0.a.$ === 'Bit32') {
					if (_n0.b.b) {
						if ((_n0.b.b.b && _n0.b.b.b.b) && _n0.b.b.b.b.b) {
							var _n1 = _n0.a;
							var _n2 = _n0.b;
							var x3 = _n2.a;
							var _n3 = _n2.b;
							var x2 = _n3.a;
							var _n4 = _n3.b;
							var x1 = _n4.a;
							var _n5 = _n4.b;
							var x0 = _n5.a;
							var rest = _n5.b;
							var acc2 = A2(
								elm$core$Array$push,
								ktonon$elm_word$Word$W(
									ktonon$elm_word$Word$int32FromBytes(
										A4(ktonon$elm_word$Word$FourBytes, x3, x2, x1, x0))),
								acc);
							var $temp$wordSize = wordSize,
								$temp$bytes = rest,
								$temp$acc = acc2;
							wordSize = $temp$wordSize;
							bytes = $temp$bytes;
							acc = $temp$acc;
							continue accWords;
						} else {
							var _n15 = _n0.a;
							var rest = _n0.b;
							return A2(
								elm$core$Array$push,
								ktonon$elm_word$Word$W(
									ktonon$elm_word$Word$int32FromBytes(
										ktonon$elm_word$Word$pad4(rest))),
								acc);
						}
					} else {
						break _n0$2;
					}
				} else {
					if (_n0.b.b) {
						if ((((((_n0.b.b.b && _n0.b.b.b.b) && _n0.b.b.b.b.b) && _n0.b.b.b.b.b.b) && _n0.b.b.b.b.b.b.b) && _n0.b.b.b.b.b.b.b.b) && _n0.b.b.b.b.b.b.b.b.b) {
							var _n6 = _n0.a;
							var _n7 = _n0.b;
							var x7 = _n7.a;
							var _n8 = _n7.b;
							var x6 = _n8.a;
							var _n9 = _n8.b;
							var x5 = _n9.a;
							var _n10 = _n9.b;
							var x4 = _n10.a;
							var _n11 = _n10.b;
							var x3 = _n11.a;
							var _n12 = _n11.b;
							var x2 = _n12.a;
							var _n13 = _n12.b;
							var x1 = _n13.a;
							var _n14 = _n13.b;
							var x0 = _n14.a;
							var rest = _n14.b;
							var acc2 = A2(
								elm$core$Array$push,
								A2(
									ktonon$elm_word$Word$D,
									ktonon$elm_word$Word$int32FromBytes(
										A4(ktonon$elm_word$Word$FourBytes, x7, x6, x5, x4)),
									ktonon$elm_word$Word$int32FromBytes(
										A4(ktonon$elm_word$Word$FourBytes, x3, x2, x1, x0))),
								acc);
							var $temp$wordSize = wordSize,
								$temp$bytes = rest,
								$temp$acc = acc2;
							wordSize = $temp$wordSize;
							bytes = $temp$bytes;
							acc = $temp$acc;
							continue accWords;
						} else {
							var _n16 = _n0.a;
							var rest = _n0.b;
							return A2(
								elm$core$Array$push,
								A2(
									ktonon$elm_word$Word$D,
									ktonon$elm_word$Word$int32FromBytes(
										ktonon$elm_word$Word$pad4(
											A2(elm$core$List$take, 4, rest))),
									ktonon$elm_word$Word$int32FromBytes(
										ktonon$elm_word$Word$pad4(
											A2(elm$core$List$drop, 4, rest)))),
								acc);
						}
					} else {
						break _n0$2;
					}
				}
			}
			return acc;
		}
	});
var ktonon$elm_word$Word$fromBytes = F2(
	function (wordSize, bytes) {
		return A3(ktonon$elm_word$Word$accWords, wordSize, bytes, elm$core$Array$empty);
	});
var ktonon$elm_crypto$Crypto$SHA$digest = function (alg) {
	return A2(
		elm$core$Basics$composeR,
		ktonon$elm_crypto$Crypto$SHA$Preprocess$preprocess(alg),
		A2(
			elm$core$Basics$composeR,
			ktonon$elm_word$Word$fromBytes(
				ktonon$elm_crypto$Crypto$SHA$Alg$wordSize(alg)),
			ktonon$elm_crypto$Crypto$SHA$Process$chunks(alg)));
};
var elm$core$String$foldr = _String_foldr;
var elm$core$String$toList = function (string) {
	return A3(elm$core$String$foldr, elm$core$List$cons, _List_Nil, string);
};
var elm$core$Bitwise$or = _Bitwise_or;
var ktonon$elm_word$Word$Bytes$splitUtf8 = function (x) {
	return (x < 128) ? _List_fromArray(
		[x]) : ((x < 2048) ? _List_fromArray(
		[192 | ((1984 & x) >>> 6), 128 | (63 & x)]) : _List_fromArray(
		[224 | ((61440 & x) >>> 12), 128 | ((4032 & x) >>> 6), 128 | (63 & x)]));
};
var ktonon$elm_word$Word$Bytes$fromUTF8 = A2(
	elm$core$Basics$composeR,
	elm$core$String$toList,
	A2(
		elm$core$List$foldl,
		F2(
			function (_char, acc) {
				return A2(
					elm$core$List$append,
					acc,
					ktonon$elm_word$Word$Bytes$splitUtf8(
						elm$core$Char$toCode(_char)));
			}),
		_List_Nil));
var elm$core$Array$foldl = F3(
	function (func, baseCase, _n0) {
		var tree = _n0.c;
		var tail = _n0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3(elm$core$Elm$JsArray$foldl, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3(elm$core$Elm$JsArray$foldl, func, acc, values);
				}
			});
		return A3(
			elm$core$Elm$JsArray$foldl,
			func,
			A3(elm$core$Elm$JsArray$foldl, helper, baseCase, tree),
			tail);
	});
var ktonon$elm_word$Word$Hex$fromArray = function (toHex) {
	return A2(
		elm$core$Array$foldl,
		F2(
			function (val, acc) {
				return _Utils_ap(
					acc,
					toHex(val));
			}),
		'');
};
var elm$core$Char$fromCode = _Char_fromCode;
var elm$core$String$cons = _String_cons;
var ktonon$elm_word$Word$Hex$fromIntAccumulator = function (x) {
	return elm$core$String$cons(
		elm$core$Char$fromCode(
			(x < 10) ? (x + 48) : ((x + 97) - 10)));
};
var ktonon$elm_word$Word$Hex$fromInt = F2(
	function (charCount, value) {
		return A3(
			elm$core$List$foldl,
			function (i) {
				return ktonon$elm_word$Word$Hex$fromIntAccumulator(
					15 & (value >>> (i * A2(elm$core$Basics$pow, 2, 2))));
			},
			'',
			A2(elm$core$List$range, 0, charCount - 1));
	});
var ktonon$elm_word$Word$Hex$fromWord = function (word) {
	switch (word.$) {
		case 'W':
			var x = word.a;
			return A2(ktonon$elm_word$Word$Hex$fromInt, 8, x);
		case 'D':
			var h = word.a;
			var l = word.b;
			return _Utils_ap(
				A2(ktonon$elm_word$Word$Hex$fromInt, 8, h),
				A2(ktonon$elm_word$Word$Hex$fromInt, 8, l));
		default:
			return 'M';
	}
};
var ktonon$elm_word$Word$Hex$fromWordArray = ktonon$elm_word$Word$Hex$fromArray(ktonon$elm_word$Word$Hex$fromWord);
var ktonon$elm_crypto$Crypto$Hash$sha256 = function (message) {
	return ktonon$elm_word$Word$Hex$fromWordArray(
		A2(
			ktonon$elm_crypto$Crypto$SHA$digest,
			ktonon$elm_crypto$Crypto$SHA$Alg$SHA256,
			ktonon$elm_word$Word$Bytes$fromUTF8(message)));
};
var author$project$Main$encodeAccount = F2(
	function (username, password) {
		return elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'username',
					elm$json$Json$Encode$string(username)),
					_Utils_Tuple2(
					'password',
					elm$json$Json$Encode$string(
						ktonon$elm_crypto$Crypto$Hash$sha256(password)))
				]));
	});
var author$project$Main$server = 'http://localhost:3000';
var elm$core$Dict$RBEmpty_elm_builtin = {$: 'RBEmpty_elm_builtin'};
var elm$core$Dict$empty = elm$core$Dict$RBEmpty_elm_builtin;
var elm$core$Basics$compare = _Utils_compare;
var elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _n1 = A2(elm$core$Basics$compare, targetKey, key);
				switch (_n1.$) {
					case 'LT':
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 'EQ':
						return elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var elm$core$Dict$Black = {$: 'Black'};
var elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: 'RBNode_elm_builtin', a: a, b: b, c: c, d: d, e: e};
	});
var elm$core$Dict$Red = {$: 'Red'};
var elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Red')) {
			var _n1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
				var _n3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Red,
					key,
					value,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, rK, rV, rLeft, rRight));
			} else {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) && (left.d.$ === 'RBNode_elm_builtin')) && (left.d.a.$ === 'Red')) {
				var _n5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _n6 = left.d;
				var _n7 = _n6.a;
				var llK = _n6.b;
				var llV = _n6.c;
				var llLeft = _n6.d;
				var llRight = _n6.e;
				var lRight = left.e;
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Red,
					lK,
					lV,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, llK, llV, llLeft, llRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, key, value, lRight, right));
			} else {
				return A5(elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, key, value, elm$core$Dict$RBEmpty_elm_builtin, elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _n1 = A2(elm$core$Basics$compare, key, nKey);
			switch (_n1.$) {
				case 'LT':
					return A5(
						elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3(elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 'EQ':
					return A5(elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3(elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _n0 = A3(elm$core$Dict$insertHelp, key, value, dict);
		if ((_n0.$ === 'RBNode_elm_builtin') && (_n0.a.$ === 'Red')) {
			var _n1 = _n0.a;
			var k = _n0.b;
			var v = _n0.c;
			var l = _n0.d;
			var r = _n0.e;
			return A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _n0;
			return x;
		}
	});
var elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
			var left = dict.d;
			var $temp$dict = left;
			dict = $temp$dict;
			continue getMin;
		} else {
			return dict;
		}
	}
};
var elm$core$Dict$moveRedLeft = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.e.d.$ === 'RBNode_elm_builtin') && (dict.e.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _n1 = dict.d;
			var lClr = _n1.a;
			var lK = _n1.b;
			var lV = _n1.c;
			var lLeft = _n1.d;
			var lRight = _n1.e;
			var _n2 = dict.e;
			var rClr = _n2.a;
			var rK = _n2.b;
			var rV = _n2.c;
			var rLeft = _n2.d;
			var _n3 = rLeft.a;
			var rlK = rLeft.b;
			var rlV = rLeft.c;
			var rlL = rLeft.d;
			var rlR = rLeft.e;
			var rRight = _n2.e;
			return A5(
				elm$core$Dict$RBNode_elm_builtin,
				elm$core$Dict$Red,
				rlK,
				rlV,
				A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Black,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, lK, lV, lLeft, lRight),
					rlL),
				A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, rK, rV, rlR, rRight));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _n4 = dict.d;
			var lClr = _n4.a;
			var lK = _n4.b;
			var lV = _n4.c;
			var lLeft = _n4.d;
			var lRight = _n4.e;
			var _n5 = dict.e;
			var rClr = _n5.a;
			var rK = _n5.b;
			var rV = _n5.c;
			var rLeft = _n5.d;
			var rRight = _n5.e;
			if (clr.$ === 'Black') {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Black,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Black,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.d.d.$ === 'RBNode_elm_builtin') && (dict.d.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _n1 = dict.d;
			var lClr = _n1.a;
			var lK = _n1.b;
			var lV = _n1.c;
			var _n2 = _n1.d;
			var _n3 = _n2.a;
			var llK = _n2.b;
			var llV = _n2.c;
			var llLeft = _n2.d;
			var llRight = _n2.e;
			var lRight = _n1.e;
			var _n4 = dict.e;
			var rClr = _n4.a;
			var rK = _n4.b;
			var rV = _n4.c;
			var rLeft = _n4.d;
			var rRight = _n4.e;
			return A5(
				elm$core$Dict$RBNode_elm_builtin,
				elm$core$Dict$Red,
				lK,
				lV,
				A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, llK, llV, llLeft, llRight),
				A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Black,
					k,
					v,
					lRight,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, rK, rV, rLeft, rRight)));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _n5 = dict.d;
			var lClr = _n5.a;
			var lK = _n5.b;
			var lV = _n5.c;
			var lLeft = _n5.d;
			var lRight = _n5.e;
			var _n6 = dict.e;
			var rClr = _n6.a;
			var rK = _n6.b;
			var rV = _n6.c;
			var rLeft = _n6.d;
			var rRight = _n6.e;
			if (clr.$ === 'Black') {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Black,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Black,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
			var _n1 = left.a;
			var lK = left.b;
			var lV = left.c;
			var lLeft = left.d;
			var lRight = left.e;
			return A5(
				elm$core$Dict$RBNode_elm_builtin,
				color,
				lK,
				lV,
				lLeft,
				A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, key, value, lRight, right));
		} else {
			_n2$2:
			while (true) {
				if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Black')) {
					if (right.d.$ === 'RBNode_elm_builtin') {
						if (right.d.a.$ === 'Black') {
							var _n3 = right.a;
							var _n4 = right.d;
							var _n5 = _n4.a;
							return elm$core$Dict$moveRedRight(dict);
						} else {
							break _n2$2;
						}
					} else {
						var _n6 = right.a;
						var _n7 = right.d;
						return elm$core$Dict$moveRedRight(dict);
					}
				} else {
					break _n2$2;
				}
			}
			return dict;
		}
	});
var elm$core$Dict$removeMin = function (dict) {
	if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor.$ === 'Black') {
			if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
				var _n3 = lLeft.a;
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					color,
					key,
					value,
					elm$core$Dict$removeMin(left),
					right);
			} else {
				var _n4 = elm$core$Dict$moveRedLeft(dict);
				if (_n4.$ === 'RBNode_elm_builtin') {
					var nColor = _n4.a;
					var nKey = _n4.b;
					var nValue = _n4.c;
					var nLeft = _n4.d;
					var nRight = _n4.e;
					return A5(
						elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						elm$core$Dict$removeMin(nLeft),
						nRight);
				} else {
					return elm$core$Dict$RBEmpty_elm_builtin;
				}
			}
		} else {
			return A5(
				elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				value,
				elm$core$Dict$removeMin(left),
				right);
		}
	} else {
		return elm$core$Dict$RBEmpty_elm_builtin;
	}
};
var elm$core$Dict$removeHelp = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Black')) {
					var _n4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
						var _n6 = lLeft.a;
						return A5(
							elm$core$Dict$RBNode_elm_builtin,
							color,
							key,
							value,
							A2(elm$core$Dict$removeHelp, targetKey, left),
							right);
					} else {
						var _n7 = elm$core$Dict$moveRedLeft(dict);
						if (_n7.$ === 'RBNode_elm_builtin') {
							var nColor = _n7.a;
							var nKey = _n7.b;
							var nValue = _n7.c;
							var nLeft = _n7.d;
							var nRight = _n7.e;
							return A5(
								elm$core$Dict$balance,
								nColor,
								nKey,
								nValue,
								A2(elm$core$Dict$removeHelp, targetKey, nLeft),
								nRight);
						} else {
							return elm$core$Dict$RBEmpty_elm_builtin;
						}
					}
				} else {
					return A5(
						elm$core$Dict$RBNode_elm_builtin,
						color,
						key,
						value,
						A2(elm$core$Dict$removeHelp, targetKey, left),
						right);
				}
			} else {
				return A2(
					elm$core$Dict$removeHelpEQGT,
					targetKey,
					A7(elm$core$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
			}
		}
	});
var elm$core$Dict$removeHelpEQGT = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBNode_elm_builtin') {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _n1 = elm$core$Dict$getMin(right);
				if (_n1.$ === 'RBNode_elm_builtin') {
					var minKey = _n1.b;
					var minValue = _n1.c;
					return A5(
						elm$core$Dict$balance,
						color,
						minKey,
						minValue,
						left,
						elm$core$Dict$removeMin(right));
				} else {
					return elm$core$Dict$RBEmpty_elm_builtin;
				}
			} else {
				return A5(
					elm$core$Dict$balance,
					color,
					key,
					value,
					left,
					A2(elm$core$Dict$removeHelp, targetKey, right));
			}
		} else {
			return elm$core$Dict$RBEmpty_elm_builtin;
		}
	});
var elm$core$Dict$remove = F2(
	function (key, dict) {
		var _n0 = A2(elm$core$Dict$removeHelp, key, dict);
		if ((_n0.$ === 'RBNode_elm_builtin') && (_n0.a.$ === 'Red')) {
			var _n1 = _n0.a;
			var k = _n0.b;
			var v = _n0.c;
			var l = _n0.d;
			var r = _n0.e;
			return A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _n0;
			return x;
		}
	});
var elm$core$Dict$update = F3(
	function (targetKey, alter, dictionary) {
		var _n0 = alter(
			A2(elm$core$Dict$get, targetKey, dictionary));
		if (_n0.$ === 'Just') {
			var value = _n0.a;
			return A3(elm$core$Dict$insert, targetKey, value, dictionary);
		} else {
			return A2(elm$core$Dict$remove, targetKey, dictionary);
		}
	});
var elm$core$Maybe$isJust = function (maybe) {
	if (maybe.$ === 'Just') {
		return true;
	} else {
		return false;
	}
};
var elm$core$Platform$sendToApp = _Platform_sendToApp;
var elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var elm$core$Result$map = F2(
	function (func, ra) {
		if (ra.$ === 'Ok') {
			var a = ra.a;
			return elm$core$Result$Ok(
				func(a));
		} else {
			var e = ra.a;
			return elm$core$Result$Err(e);
		}
	});
var elm$http$Http$BadStatus_ = F2(
	function (a, b) {
		return {$: 'BadStatus_', a: a, b: b};
	});
var elm$http$Http$BadUrl_ = function (a) {
	return {$: 'BadUrl_', a: a};
};
var elm$http$Http$GoodStatus_ = F2(
	function (a, b) {
		return {$: 'GoodStatus_', a: a, b: b};
	});
var elm$http$Http$NetworkError_ = {$: 'NetworkError_'};
var elm$http$Http$Receiving = function (a) {
	return {$: 'Receiving', a: a};
};
var elm$http$Http$Sending = function (a) {
	return {$: 'Sending', a: a};
};
var elm$http$Http$Timeout_ = {$: 'Timeout_'};
var elm$http$Http$expectBytesResponse = F2(
	function (toMsg, toResult) {
		return A3(
			_Http_expect,
			'arraybuffer',
			_Http_toDataView,
			A2(elm$core$Basics$composeR, toResult, toMsg));
	});
var elm$core$Result$mapError = F2(
	function (f, result) {
		if (result.$ === 'Ok') {
			var v = result.a;
			return elm$core$Result$Ok(v);
		} else {
			var e = result.a;
			return elm$core$Result$Err(
				f(e));
		}
	});
var elm$http$Http$BadBody = function (a) {
	return {$: 'BadBody', a: a};
};
var elm$http$Http$BadStatus = function (a) {
	return {$: 'BadStatus', a: a};
};
var elm$http$Http$BadUrl = function (a) {
	return {$: 'BadUrl', a: a};
};
var elm$http$Http$NetworkError = {$: 'NetworkError'};
var elm$http$Http$Timeout = {$: 'Timeout'};
var elm$http$Http$resolve = F2(
	function (toResult, response) {
		switch (response.$) {
			case 'BadUrl_':
				var url = response.a;
				return elm$core$Result$Err(
					elm$http$Http$BadUrl(url));
			case 'Timeout_':
				return elm$core$Result$Err(elm$http$Http$Timeout);
			case 'NetworkError_':
				return elm$core$Result$Err(elm$http$Http$NetworkError);
			case 'BadStatus_':
				var metadata = response.a;
				return elm$core$Result$Err(
					elm$http$Http$BadStatus(metadata.statusCode));
			default:
				var body = response.b;
				return A2(
					elm$core$Result$mapError,
					elm$http$Http$BadBody,
					toResult(body));
		}
	});
var elm$http$Http$expectWhatever = function (toMsg) {
	return A2(
		elm$http$Http$expectBytesResponse,
		toMsg,
		elm$http$Http$resolve(
			function (_n0) {
				return elm$core$Result$Ok(_Utils_Tuple0);
			}));
};
var elm$http$Http$jsonBody = function (value) {
	return A2(
		_Http_pair,
		'application/json',
		A2(elm$json$Json$Encode$encode, 0, value));
};
var elm$http$Http$Request = function (a) {
	return {$: 'Request', a: a};
};
var elm$core$Task$succeed = _Scheduler_succeed;
var elm$http$Http$State = F2(
	function (reqs, subs) {
		return {reqs: reqs, subs: subs};
	});
var elm$http$Http$init = elm$core$Task$succeed(
	A2(elm$http$Http$State, elm$core$Dict$empty, _List_Nil));
var elm$core$Task$andThen = _Scheduler_andThen;
var elm$core$Process$kill = _Scheduler_kill;
var elm$core$Process$spawn = _Scheduler_spawn;
var elm$http$Http$updateReqs = F3(
	function (router, cmds, reqs) {
		updateReqs:
		while (true) {
			if (!cmds.b) {
				return elm$core$Task$succeed(reqs);
			} else {
				var cmd = cmds.a;
				var otherCmds = cmds.b;
				if (cmd.$ === 'Cancel') {
					var tracker = cmd.a;
					var _n2 = A2(elm$core$Dict$get, tracker, reqs);
					if (_n2.$ === 'Nothing') {
						var $temp$router = router,
							$temp$cmds = otherCmds,
							$temp$reqs = reqs;
						router = $temp$router;
						cmds = $temp$cmds;
						reqs = $temp$reqs;
						continue updateReqs;
					} else {
						var pid = _n2.a;
						return A2(
							elm$core$Task$andThen,
							function (_n3) {
								return A3(
									elm$http$Http$updateReqs,
									router,
									otherCmds,
									A2(elm$core$Dict$remove, tracker, reqs));
							},
							elm$core$Process$kill(pid));
					}
				} else {
					var req = cmd.a;
					return A2(
						elm$core$Task$andThen,
						function (pid) {
							var _n4 = req.tracker;
							if (_n4.$ === 'Nothing') {
								return A3(elm$http$Http$updateReqs, router, otherCmds, reqs);
							} else {
								var tracker = _n4.a;
								return A3(
									elm$http$Http$updateReqs,
									router,
									otherCmds,
									A3(elm$core$Dict$insert, tracker, pid, reqs));
							}
						},
						elm$core$Process$spawn(
							A3(
								_Http_toTask,
								router,
								elm$core$Platform$sendToApp(router),
								req)));
				}
			}
		}
	});
var elm$http$Http$onEffects = F4(
	function (router, cmds, subs, state) {
		return A2(
			elm$core$Task$andThen,
			function (reqs) {
				return elm$core$Task$succeed(
					A2(elm$http$Http$State, reqs, subs));
			},
			A3(elm$http$Http$updateReqs, router, cmds, state.reqs));
	});
var elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _n0 = f(mx);
		if (_n0.$ === 'Just') {
			var x = _n0.a;
			return A2(elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			elm$core$List$foldr,
			elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			elm$core$Task$andThen,
			function (a) {
				return A2(
					elm$core$Task$andThen,
					function (b) {
						return elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var elm$core$Task$sequence = function (tasks) {
	return A3(
		elm$core$List$foldr,
		elm$core$Task$map2(elm$core$List$cons),
		elm$core$Task$succeed(_List_Nil),
		tasks);
};
var elm$http$Http$maybeSend = F4(
	function (router, desiredTracker, progress, _n0) {
		var actualTracker = _n0.a;
		var toMsg = _n0.b;
		return _Utils_eq(desiredTracker, actualTracker) ? elm$core$Maybe$Just(
			A2(
				elm$core$Platform$sendToApp,
				router,
				toMsg(progress))) : elm$core$Maybe$Nothing;
	});
var elm$http$Http$onSelfMsg = F3(
	function (router, _n0, state) {
		var tracker = _n0.a;
		var progress = _n0.b;
		return A2(
			elm$core$Task$andThen,
			function (_n1) {
				return elm$core$Task$succeed(state);
			},
			elm$core$Task$sequence(
				A2(
					elm$core$List$filterMap,
					A3(elm$http$Http$maybeSend, router, tracker, progress),
					state.subs)));
	});
var elm$http$Http$Cancel = function (a) {
	return {$: 'Cancel', a: a};
};
var elm$http$Http$cmdMap = F2(
	function (func, cmd) {
		if (cmd.$ === 'Cancel') {
			var tracker = cmd.a;
			return elm$http$Http$Cancel(tracker);
		} else {
			var r = cmd.a;
			return elm$http$Http$Request(
				{
					allowCookiesFromOtherDomains: r.allowCookiesFromOtherDomains,
					body: r.body,
					expect: A2(_Http_mapExpect, func, r.expect),
					headers: r.headers,
					method: r.method,
					timeout: r.timeout,
					tracker: r.tracker,
					url: r.url
				});
		}
	});
var elm$http$Http$MySub = F2(
	function (a, b) {
		return {$: 'MySub', a: a, b: b};
	});
var elm$http$Http$subMap = F2(
	function (func, _n0) {
		var tracker = _n0.a;
		var toMsg = _n0.b;
		return A2(
			elm$http$Http$MySub,
			tracker,
			A2(elm$core$Basics$composeR, toMsg, func));
	});
_Platform_effectManagers['Http'] = _Platform_createManager(elm$http$Http$init, elm$http$Http$onEffects, elm$http$Http$onSelfMsg, elm$http$Http$cmdMap, elm$http$Http$subMap);
var elm$http$Http$command = _Platform_leaf('Http');
var elm$http$Http$subscription = _Platform_leaf('Http');
var elm$http$Http$request = function (r) {
	return elm$http$Http$command(
		elm$http$Http$Request(
			{allowCookiesFromOtherDomains: false, body: r.body, expect: r.expect, headers: r.headers, method: r.method, timeout: r.timeout, tracker: r.tracker, url: r.url}));
};
var elm$http$Http$post = function (r) {
	return elm$http$Http$request(
		{body: r.body, expect: r.expect, headers: _List_Nil, method: 'POST', timeout: elm$core$Maybe$Nothing, tracker: elm$core$Maybe$Nothing, url: r.url});
};
var author$project$Main$login = F2(
	function (username, password) {
		return elm$http$Http$post(
			{
				body: elm$http$Http$jsonBody(
					A2(author$project$Main$encodeAccount, username, password)),
				expect: elm$http$Http$expectWhatever(author$project$Main$LoginResponse),
				url: author$project$Main$server + '/sign_in'
			});
	});
var author$project$Main$OTPResponse = function (a) {
	return {$: 'OTPResponse', a: a};
};
var elm$core$String$toInt = _String_toInt;
var elm$json$Json$Encode$int = _Json_wrap;
var author$project$Main$encodeOtp = F2(
	function (username, code) {
		return elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'username',
					elm$json$Json$Encode$string(username)),
					function () {
					var _n0 = elm$core$String$toInt(code);
					if (_n0.$ === 'Just') {
						var _int = _n0.a;
						return _Utils_Tuple2(
							'otp',
							elm$json$Json$Encode$int(_int));
					} else {
						return _Utils_Tuple2(
							'otp',
							elm$json$Json$Encode$string(code));
					}
				}()
				]));
	});
var author$project$Main$otp = F2(
	function (username, code) {
		return elm$http$Http$post(
			{
				body: elm$http$Http$jsonBody(
					A2(author$project$Main$encodeOtp, username, code)),
				expect: elm$http$Http$expectWhatever(author$project$Main$OTPResponse),
				url: author$project$Main$server + '/validate'
			});
	});
var author$project$Main$RegistrationResponse = function (a) {
	return {$: 'RegistrationResponse', a: a};
};
var elm$core$Basics$identity = function (x) {
	return x;
};
var elm$http$Http$expectStringResponse = F2(
	function (toMsg, toResult) {
		return A3(
			_Http_expect,
			'',
			elm$core$Basics$identity,
			A2(elm$core$Basics$composeR, toResult, toMsg));
	});
var elm$json$Json$Decode$decodeString = _Json_runOnString;
var elm$http$Http$expectJson = F2(
	function (toMsg, decoder) {
		return A2(
			elm$http$Http$expectStringResponse,
			toMsg,
			elm$http$Http$resolve(
				function (string) {
					return A2(
						elm$core$Result$mapError,
						elm$json$Json$Decode$errorToString,
						A2(elm$json$Json$Decode$decodeString, decoder, string));
				}));
	});
var elm$json$Json$Decode$field = _Json_decodeField;
var elm$json$Json$Decode$string = _Json_decodeString;
var author$project$Main$register = F2(
	function (username, password) {
		return elm$http$Http$post(
			{
				body: elm$http$Http$jsonBody(
					A2(author$project$Main$encodeAccount, username, password)),
				expect: A2(
					elm$http$Http$expectJson,
					author$project$Main$RegistrationResponse,
					A2(elm$json$Json$Decode$field, 'secret', elm$json$Json$Decode$string)),
				url: author$project$Main$server + '/sign_up'
			});
	});
var elm$core$Basics$neq = _Utils_notEqual;
var elm$core$String$length = _String_length;
var author$project$Main$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'Name':
				var name = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{name: name}),
					elm$core$Platform$Cmd$none);
			case 'Password':
				var password = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{password: password}),
					elm$core$Platform$Cmd$none);
			case 'Code':
				var code = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{code: code}),
					elm$core$Platform$Cmd$none);
			case 'SwitchMode':
				var mode = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{code: '', mode: mode, name: '', password: '', secret: author$project$Main$NotReceived, state: author$project$Main$NoLogin, warning: ''}),
					elm$core$Platform$Cmd$none);
			case 'RegistrationResponse':
				var response = msg.a;
				if (response.$ === 'Ok') {
					var string = response.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								secret: author$project$Main$Received(string),
								warning: ''
							}),
						elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{warning: 'Failure'}),
						elm$core$Platform$Cmd$none);
				}
			case 'LoginResponse':
				var response = msg.a;
				if (response.$ === 'Ok') {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								state: author$project$Main$AwaitOTP(model.name),
								warning: ''
							}),
						elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{warning: 'Login failed'}),
						elm$core$Platform$Cmd$none);
				}
			case 'OTPResponse':
				var response = msg.a;
				if (response.$ === 'Ok') {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								state: author$project$Main$Login(model.name),
								warning: ''
							}),
						elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{warning: 'Verification failed'}),
						elm$core$Platform$Cmd$none);
				}
			case 'LogIn':
				return ((model.name !== '') && (model.password !== '')) ? _Utils_Tuple2(
					model,
					A2(author$project$Main$login, model.name, model.password)) : _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
			case 'Register':
				return ((model.name !== '') && ((model.password !== '') && (elm$core$String$length(model.password) > 5))) ? _Utils_Tuple2(
					model,
					A2(author$project$Main$register, model.name, model.password)) : _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
			default:
				return (model.code !== '') ? _Utils_Tuple2(
					model,
					A2(author$project$Main$otp, model.name, model.code)) : _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
		}
	});
var author$project$Main$Code = function (a) {
	return {$: 'Code', a: a};
};
var author$project$Main$LogIn = {$: 'LogIn'};
var author$project$Main$Name = function (a) {
	return {$: 'Name', a: a};
};
var author$project$Main$OTP = {$: 'OTP'};
var author$project$Main$Password = function (a) {
	return {$: 'Password', a: a};
};
var author$project$Main$Register = {$: 'Register'};
var author$project$Main$SignIn = {$: 'SignIn'};
var author$project$Main$SignUp = {$: 'SignUp'};
var author$project$Main$SwitchMode = function (a) {
	return {$: 'SwitchMode', a: a};
};
var elm$core$Result$withDefault = F2(
	function (def, result) {
		if (result.$ === 'Ok') {
			var a = result.a;
			return a;
		} else {
			return def;
		}
	});
var elm$json$Json$Decode$map = _Json_map1;
var elm$json$Json$Decode$map2 = _Json_map2;
var elm$json$Json$Decode$succeed = _Json_succeed;
var elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 'Normal':
			return 0;
		case 'MayStopPropagation':
			return 1;
		case 'MayPreventDefault':
			return 2;
		default:
			return 3;
	}
};
var elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var elm$html$Html$text = elm$virtual_dom$VirtualDom$text;
var pablohirafuji$elm_qrcode$QRCode$Quartile = {$: 'Quartile'};
var elm$core$Result$andThen = F2(
	function (callback, result) {
		if (result.$ === 'Ok') {
			var value = result.a;
			return callback(value);
		} else {
			var msg = result.a;
			return elm$core$Result$Err(msg);
		}
	});
var pablohirafuji$elm_qrcode$QRCode$QRCode = function (a) {
	return {$: 'QRCode', a: a};
};
var pablohirafuji$elm_qrcode$QRCode$ECLevel$H = {$: 'H'};
var pablohirafuji$elm_qrcode$QRCode$ECLevel$L = {$: 'L'};
var pablohirafuji$elm_qrcode$QRCode$ECLevel$M = {$: 'M'};
var pablohirafuji$elm_qrcode$QRCode$ECLevel$Q = {$: 'Q'};
var pablohirafuji$elm_qrcode$QRCode$convertEC = function (ec) {
	switch (ec.$) {
		case 'Low':
			return pablohirafuji$elm_qrcode$QRCode$ECLevel$L;
		case 'Medium':
			return pablohirafuji$elm_qrcode$QRCode$ECLevel$M;
		case 'Quartile':
			return pablohirafuji$elm_qrcode$QRCode$ECLevel$Q;
		default:
			return pablohirafuji$elm_qrcode$QRCode$ECLevel$H;
	}
};
var pablohirafuji$elm_qrcode$QRCode$AlignmentPatternNotFound = {$: 'AlignmentPatternNotFound'};
var pablohirafuji$elm_qrcode$QRCode$InputLengthOverflow = {$: 'InputLengthOverflow'};
var pablohirafuji$elm_qrcode$QRCode$InvalidAlphanumericChar = {$: 'InvalidAlphanumericChar'};
var pablohirafuji$elm_qrcode$QRCode$InvalidNumericChar = {$: 'InvalidNumericChar'};
var pablohirafuji$elm_qrcode$QRCode$InvalidUTF8Char = {$: 'InvalidUTF8Char'};
var pablohirafuji$elm_qrcode$QRCode$LogTableException = function (a) {
	return {$: 'LogTableException', a: a};
};
var pablohirafuji$elm_qrcode$QRCode$PolynomialModException = {$: 'PolynomialModException'};
var pablohirafuji$elm_qrcode$QRCode$PolynomialMultiplyException = {$: 'PolynomialMultiplyException'};
var pablohirafuji$elm_qrcode$QRCode$convertError = function (e) {
	switch (e.$) {
		case 'AlignmentPatternNotFound':
			return pablohirafuji$elm_qrcode$QRCode$AlignmentPatternNotFound;
		case 'InvalidNumericChar':
			return pablohirafuji$elm_qrcode$QRCode$InvalidNumericChar;
		case 'InvalidAlphanumericChar':
			return pablohirafuji$elm_qrcode$QRCode$InvalidAlphanumericChar;
		case 'InvalidUTF8Char':
			return pablohirafuji$elm_qrcode$QRCode$InvalidUTF8Char;
		case 'LogTableException':
			var n = e.a;
			return pablohirafuji$elm_qrcode$QRCode$LogTableException(n);
		case 'PolynomialMultiplyException':
			return pablohirafuji$elm_qrcode$QRCode$PolynomialMultiplyException;
		case 'PolynomialModException':
			return pablohirafuji$elm_qrcode$QRCode$PolynomialModException;
		default:
			return pablohirafuji$elm_qrcode$QRCode$InputLengthOverflow;
	}
};
var pablohirafuji$elm_qrcode$QRCode$Encode$firstFillerByte = 236;
var pablohirafuji$elm_qrcode$QRCode$Encode$secondFillerByte = 17;
var pablohirafuji$elm_qrcode$QRCode$Encode$addFiller = F2(
	function (capacity, bytes) {
		var fillerLength = ((capacity / 8) | 0) - elm$core$List$length(bytes);
		var ns = elm$core$List$concat(
			A2(
				elm$core$List$repeat,
				(fillerLength / 2) | 0,
				_List_fromArray(
					[pablohirafuji$elm_qrcode$QRCode$Encode$firstFillerByte, pablohirafuji$elm_qrcode$QRCode$Encode$secondFillerByte])));
		return (!A2(elm$core$Basics$modBy, 2, fillerLength)) ? _Utils_ap(bytes, ns) : _Utils_ap(
			bytes,
			_Utils_ap(
				ns,
				_List_fromArray(
					[pablohirafuji$elm_qrcode$QRCode$Encode$firstFillerByte])));
	});
var elm$core$Basics$min = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) < 0) ? x : y;
	});
var pablohirafuji$elm_qrcode$QRCode$Encode$addTerminator = F3(
	function (capacity, bitsCount, bits) {
		return _Utils_ap(
			bits,
			_List_fromArray(
				[
					_Utils_Tuple2(
					0,
					A2(elm$core$Basics$min, 4, capacity - bitsCount))
				]));
	});
var elm$core$Bitwise$shiftRightBy = _Bitwise_shiftRightBy;
var pablohirafuji$elm_qrcode$QRCode$Encode$bitsToBytes3 = function (_n0) {
	bitsToBytes3:
	while (true) {
		var _n1 = _n0.a;
		var bits = _n1.a;
		var length = _n1.b;
		var bytes = _n0.b;
		if (length >= 8) {
			var remLength = length - 8;
			var remBits = bits & ((1 << remLength) - 1);
			var _byte = bits >> remLength;
			var $temp$_n0 = _Utils_Tuple2(
				_Utils_Tuple2(remBits, remLength),
				A2(elm$core$List$cons, _byte, bytes));
			_n0 = $temp$_n0;
			continue bitsToBytes3;
		} else {
			return _Utils_Tuple2(
				_Utils_Tuple2(bits, length),
				bytes);
		}
	}
};
var pablohirafuji$elm_qrcode$QRCode$Encode$bitsToBytes2 = F2(
	function (_n0, _n1) {
		var curBits = _n0.a;
		var curLength = _n0.b;
		var _n2 = _n1.a;
		var remBits = _n2.a;
		var remLength = _n2.b;
		var bytes = _n1.b;
		var lengthSum = curLength + remLength;
		var bitsSum = curBits | (remBits << curLength);
		return pablohirafuji$elm_qrcode$QRCode$Encode$bitsToBytes3(
			_Utils_Tuple2(
				_Utils_Tuple2(bitsSum, lengthSum),
				bytes));
	});
var pablohirafuji$elm_qrcode$QRCode$Encode$bitsToBytes1 = F2(
	function (bits, _n0) {
		bitsToBytes1:
		while (true) {
			var _n1 = _n0.a;
			var remBits = _n1.a;
			var remLength = _n1.b;
			var bytes = _n0.b;
			if (bits.b) {
				var head = bits.a;
				var tail = bits.b;
				var $temp$bits = tail,
					$temp$_n0 = A2(
					pablohirafuji$elm_qrcode$QRCode$Encode$bitsToBytes2,
					head,
					_Utils_Tuple2(
						_Utils_Tuple2(remBits, remLength),
						bytes));
				bits = $temp$bits;
				_n0 = $temp$_n0;
				continue bitsToBytes1;
			} else {
				return (!remLength) ? elm$core$List$reverse(bytes) : elm$core$List$reverse(
					A2(elm$core$List$cons, remBits << (8 - remLength), bytes));
			}
		}
	});
var pablohirafuji$elm_qrcode$QRCode$Encode$bitsToBytes = function (bits) {
	return A2(
		pablohirafuji$elm_qrcode$QRCode$Encode$bitsToBytes1,
		bits,
		_Utils_Tuple2(
			_Utils_Tuple2(0, 0),
			_List_Nil));
};
var pablohirafuji$elm_qrcode$QRCode$Encode$UTF8 = {$: 'UTF8'};
var pablohirafuji$elm_qrcode$QRCode$Encode$charCountIndicatorLength = F2(
	function (mode, version) {
		if (version <= 9) {
			switch (mode.$) {
				case 'Numeric':
					return 10;
				case 'Alphanumeric':
					return 9;
				case 'Byte':
					return 8;
				default:
					return 8;
			}
		} else {
			if (version <= 26) {
				switch (mode.$) {
					case 'Numeric':
						return 12;
					case 'Alphanumeric':
						return 11;
					case 'Byte':
						return 16;
					default:
						return 16;
				}
			} else {
				switch (mode.$) {
					case 'Numeric':
						return 14;
					case 'Alphanumeric':
						return 13;
					case 'Byte':
						return 16;
					default:
						return 16;
				}
			}
		}
	});
var pablohirafuji$elm_qrcode$QRCode$Encode$charCountIndicator = F2(
	function (_n0, bits) {
		var groupInfo = _n0.groupInfo;
		var inputStr = _n0.inputStr;
		var mode = _n0.mode;
		var length = A2(pablohirafuji$elm_qrcode$QRCode$Encode$charCountIndicatorLength, mode, groupInfo.version);
		var charCount = _Utils_eq(mode, pablohirafuji$elm_qrcode$QRCode$Encode$UTF8) ? elm$core$List$length(bits) : elm$core$String$length(inputStr);
		return _Utils_Tuple2(charCount, length);
	});
var pablohirafuji$elm_qrcode$QRCode$Encode$modeIndicator = function (mode) {
	switch (mode.$) {
		case 'Numeric':
			return 1;
		case 'Alphanumeric':
			return 2;
		case 'Byte':
			return 4;
		default:
			return 4;
	}
};
var pablohirafuji$elm_qrcode$QRCode$Encode$addInfoAndFinalBits = function (_n0) {
	var bits = _n0.a;
	var model = _n0.b;
	return _Utils_Tuple2(
		model,
		A2(
			pablohirafuji$elm_qrcode$QRCode$Encode$addFiller,
			model.groupInfo.capacity,
			pablohirafuji$elm_qrcode$QRCode$Encode$bitsToBytes(
				A3(
					pablohirafuji$elm_qrcode$QRCode$Encode$addTerminator,
					model.groupInfo.capacity,
					model.bitsCount,
					A2(
						elm$core$List$cons,
						_Utils_Tuple2(
							pablohirafuji$elm_qrcode$QRCode$Encode$modeIndicator(model.mode),
							4),
						A2(
							elm$core$List$cons,
							A2(pablohirafuji$elm_qrcode$QRCode$Encode$charCountIndicator, model, bits),
							bits))))));
};
var elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return elm$core$Maybe$Just(x);
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var elm$core$List$tail = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return elm$core$Maybe$Just(xs);
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var pablohirafuji$elm_qrcode$QRCode$Helpers$transpose = function (ll) {
	transpose:
	while (true) {
		if (!ll.b) {
			return _List_Nil;
		} else {
			if (!ll.a.b) {
				var xss = ll.b;
				var $temp$ll = xss;
				ll = $temp$ll;
				continue transpose;
			} else {
				var _n1 = ll.a;
				var x = _n1.a;
				var xs = _n1.b;
				var xss = ll.b;
				var tails = A2(elm$core$List$filterMap, elm$core$List$tail, xss);
				var heads = A2(elm$core$List$filterMap, elm$core$List$head, xss);
				return A2(
					elm$core$List$cons,
					A2(elm$core$List$cons, x, heads),
					pablohirafuji$elm_qrcode$QRCode$Helpers$transpose(
						A2(elm$core$List$cons, xs, tails)));
			}
		}
	}
};
var pablohirafuji$elm_qrcode$QRCode$Encode$concatTranspose = function (_n0) {
	var model = _n0.a;
	var dataBlocks = _n0.b;
	var ecBlocks = _n0.c;
	return _Utils_Tuple2(
		model,
		elm$core$List$concat(
			_Utils_ap(
				pablohirafuji$elm_qrcode$QRCode$Helpers$transpose(dataBlocks),
				pablohirafuji$elm_qrcode$QRCode$Helpers$transpose(ecBlocks))));
};
var elm$core$Result$map2 = F3(
	function (func, ra, rb) {
		if (ra.$ === 'Err') {
			var x = ra.a;
			return elm$core$Result$Err(x);
		} else {
			var a = ra.a;
			if (rb.$ === 'Err') {
				var x = rb.a;
				return elm$core$Result$Err(x);
			} else {
				var b = rb.a;
				return elm$core$Result$Ok(
					A2(func, a, b));
			}
		}
	});
var elm_community$list_extra$List$Extra$greedyGroupsOfWithStep = F3(
	function (size, step, xs) {
		var xs_ = A2(elm$core$List$drop, step, xs);
		var okayXs = elm$core$List$length(xs) > 0;
		var okayArgs = (size > 0) && (step > 0);
		return (okayArgs && okayXs) ? A2(
			elm$core$List$cons,
			A2(elm$core$List$take, size, xs),
			A3(elm_community$list_extra$List$Extra$greedyGroupsOfWithStep, size, step, xs_)) : _List_Nil;
	});
var elm_community$list_extra$List$Extra$greedyGroupsOf = F2(
	function (size, xs) {
		return A3(elm_community$list_extra$List$Extra$greedyGroupsOfWithStep, size, size, xs);
	});
var elm$core$Result$fromMaybe = F2(
	function (err, maybe) {
		if (maybe.$ === 'Just') {
			var v = maybe.a;
			return elm$core$Result$Ok(v);
		} else {
			return elm$core$Result$Err(err);
		}
	});
var elm$core$Dict$fromList = function (assocs) {
	return A3(
		elm$core$List$foldl,
		F2(
			function (_n0, dict) {
				var key = _n0.a;
				var value = _n0.b;
				return A3(elm$core$Dict$insert, key, value, dict);
			}),
		elm$core$Dict$empty,
		assocs);
};
var pablohirafuji$elm_qrcode$QRCode$Encode$Alphanumeric$alphanumericCodes = elm$core$Dict$fromList(
	_List_fromArray(
		[
			_Utils_Tuple2(
			_Utils_chr('0'),
			0),
			_Utils_Tuple2(
			_Utils_chr('1'),
			1),
			_Utils_Tuple2(
			_Utils_chr('2'),
			2),
			_Utils_Tuple2(
			_Utils_chr('3'),
			3),
			_Utils_Tuple2(
			_Utils_chr('4'),
			4),
			_Utils_Tuple2(
			_Utils_chr('5'),
			5),
			_Utils_Tuple2(
			_Utils_chr('6'),
			6),
			_Utils_Tuple2(
			_Utils_chr('7'),
			7),
			_Utils_Tuple2(
			_Utils_chr('8'),
			8),
			_Utils_Tuple2(
			_Utils_chr('9'),
			9),
			_Utils_Tuple2(
			_Utils_chr('A'),
			10),
			_Utils_Tuple2(
			_Utils_chr('B'),
			11),
			_Utils_Tuple2(
			_Utils_chr('C'),
			12),
			_Utils_Tuple2(
			_Utils_chr('D'),
			13),
			_Utils_Tuple2(
			_Utils_chr('E'),
			14),
			_Utils_Tuple2(
			_Utils_chr('F'),
			15),
			_Utils_Tuple2(
			_Utils_chr('G'),
			16),
			_Utils_Tuple2(
			_Utils_chr('H'),
			17),
			_Utils_Tuple2(
			_Utils_chr('I'),
			18),
			_Utils_Tuple2(
			_Utils_chr('J'),
			19),
			_Utils_Tuple2(
			_Utils_chr('K'),
			20),
			_Utils_Tuple2(
			_Utils_chr('L'),
			21),
			_Utils_Tuple2(
			_Utils_chr('M'),
			22),
			_Utils_Tuple2(
			_Utils_chr('N'),
			23),
			_Utils_Tuple2(
			_Utils_chr('O'),
			24),
			_Utils_Tuple2(
			_Utils_chr('P'),
			25),
			_Utils_Tuple2(
			_Utils_chr('Q'),
			26),
			_Utils_Tuple2(
			_Utils_chr('R'),
			27),
			_Utils_Tuple2(
			_Utils_chr('S'),
			28),
			_Utils_Tuple2(
			_Utils_chr('T'),
			29),
			_Utils_Tuple2(
			_Utils_chr('U'),
			30),
			_Utils_Tuple2(
			_Utils_chr('V'),
			31),
			_Utils_Tuple2(
			_Utils_chr('W'),
			32),
			_Utils_Tuple2(
			_Utils_chr('X'),
			33),
			_Utils_Tuple2(
			_Utils_chr('Y'),
			34),
			_Utils_Tuple2(
			_Utils_chr('Z'),
			35),
			_Utils_Tuple2(
			_Utils_chr(' '),
			36),
			_Utils_Tuple2(
			_Utils_chr('$'),
			37),
			_Utils_Tuple2(
			_Utils_chr('%'),
			38),
			_Utils_Tuple2(
			_Utils_chr('*'),
			39),
			_Utils_Tuple2(
			_Utils_chr('+'),
			40),
			_Utils_Tuple2(
			_Utils_chr('-'),
			41),
			_Utils_Tuple2(
			_Utils_chr('.'),
			42),
			_Utils_Tuple2(
			_Utils_chr('/'),
			43),
			_Utils_Tuple2(
			_Utils_chr(':'),
			44)
		]));
var pablohirafuji$elm_qrcode$QRCode$Error$InvalidAlphanumericChar = {$: 'InvalidAlphanumericChar'};
var pablohirafuji$elm_qrcode$QRCode$Encode$Alphanumeric$toAlphanumericCode = function (_char) {
	return A2(
		elm$core$Result$fromMaybe,
		pablohirafuji$elm_qrcode$QRCode$Error$InvalidAlphanumericChar,
		A2(elm$core$Dict$get, _char, pablohirafuji$elm_qrcode$QRCode$Encode$Alphanumeric$alphanumericCodes));
};
var pablohirafuji$elm_qrcode$QRCode$Encode$Alphanumeric$toBinary = function (chars) {
	_n0$2:
	while (true) {
		if (chars.b) {
			if (chars.b.b) {
				if (!chars.b.b.b) {
					var firstChar = chars.a;
					var _n1 = chars.b;
					var secondChar = _n1.a;
					return A3(
						elm$core$Result$map2,
						F2(
							function (firstCode, secondCode) {
								return _Utils_Tuple2((firstCode * 45) + secondCode, 11);
							}),
						pablohirafuji$elm_qrcode$QRCode$Encode$Alphanumeric$toAlphanumericCode(firstChar),
						pablohirafuji$elm_qrcode$QRCode$Encode$Alphanumeric$toAlphanumericCode(secondChar));
				} else {
					break _n0$2;
				}
			} else {
				var _char = chars.a;
				return A2(
					elm$core$Result$map,
					function (a) {
						return _Utils_Tuple2(a, 6);
					},
					pablohirafuji$elm_qrcode$QRCode$Encode$Alphanumeric$toAlphanumericCode(_char));
			}
		} else {
			break _n0$2;
		}
	}
	return elm$core$Result$Err(pablohirafuji$elm_qrcode$QRCode$Error$InvalidAlphanumericChar);
};
var pablohirafuji$elm_qrcode$QRCode$Encode$Alphanumeric$encode = function (str) {
	return A3(
		elm$core$List$foldr,
		elm$core$Result$map2(elm$core$List$cons),
		elm$core$Result$Ok(_List_Nil),
		A2(
			elm$core$List$map,
			pablohirafuji$elm_qrcode$QRCode$Encode$Alphanumeric$toBinary,
			A2(
				elm_community$list_extra$List$Extra$greedyGroupsOf,
				2,
				elm$core$String$toList(str))));
};
var pablohirafuji$elm_qrcode$QRCode$Encode$Byte$encode = function (str) {
	return elm$core$Result$Ok(
		A2(
			elm$core$List$map,
			function (a) {
				return _Utils_Tuple2(
					elm$core$Char$toCode(a),
					8);
			},
			elm$core$String$toList(str)));
};
var elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return elm$core$Maybe$Just(
				f(value));
		} else {
			return elm$core$Maybe$Nothing;
		}
	});
var elm$core$String$fromList = _String_fromList;
var pablohirafuji$elm_qrcode$QRCode$Encode$Numeric$numericLength = function (str) {
	var _n0 = elm$core$String$length(str);
	switch (_n0) {
		case 1:
			return 4;
		case 2:
			return 7;
		default:
			return 10;
	}
};
var pablohirafuji$elm_qrcode$QRCode$Error$InvalidNumericChar = {$: 'InvalidNumericChar'};
var pablohirafuji$elm_qrcode$QRCode$Encode$Numeric$encodeHelp = function (chars) {
	var str = elm$core$String$fromList(chars);
	return A2(
		elm$core$Result$fromMaybe,
		pablohirafuji$elm_qrcode$QRCode$Error$InvalidNumericChar,
		A2(
			elm$core$Maybe$map,
			function (a) {
				return _Utils_Tuple2(
					a,
					pablohirafuji$elm_qrcode$QRCode$Encode$Numeric$numericLength(str));
			},
			elm$core$String$toInt(str)));
};
var pablohirafuji$elm_qrcode$QRCode$Encode$Numeric$encode = function (str) {
	return A3(
		elm$core$List$foldr,
		elm$core$Result$map2(elm$core$List$cons),
		elm$core$Result$Ok(_List_Nil),
		A2(
			elm$core$List$map,
			pablohirafuji$elm_qrcode$QRCode$Encode$Numeric$encodeHelp,
			A2(
				elm_community$list_extra$List$Extra$greedyGroupsOf,
				3,
				elm$core$String$toList(str))));
};
var pablohirafuji$elm_qrcode$QRCode$Encode$UTF8$and63 = elm$core$Bitwise$and(63);
var pablohirafuji$elm_qrcode$QRCode$Error$InvalidUTF8Char = {$: 'InvalidUTF8Char'};
var pablohirafuji$elm_qrcode$QRCode$Encode$UTF8$encodeHelp = F2(
	function (chars, list) {
		if (chars.b) {
			var _char = chars.a;
			var charsTail = chars.b;
			return A3(
				pablohirafuji$elm_qrcode$QRCode$Encode$UTF8$utf8ToByte,
				list,
				charsTail,
				elm$core$Char$toCode(_char));
		} else {
			return elm$core$Result$Ok(
				elm$core$List$reverse(list));
		}
	});
var pablohirafuji$elm_qrcode$QRCode$Encode$UTF8$utf8ToByte = F3(
	function (list, remainStr, charCode) {
		if (charCode < 128) {
			return A2(
				pablohirafuji$elm_qrcode$QRCode$Encode$UTF8$encodeHelp,
				remainStr,
				A2(elm$core$List$cons, charCode, list));
		} else {
			if (charCode < 2048) {
				return A2(
					pablohirafuji$elm_qrcode$QRCode$Encode$UTF8$encodeHelp,
					remainStr,
					A2(
						elm$core$List$cons,
						128 | pablohirafuji$elm_qrcode$QRCode$Encode$UTF8$and63(charCode),
						A2(elm$core$List$cons, 192 | (charCode >> 6), list)));
			} else {
				if ((charCode < 55296) || (charCode >= 57344)) {
					return A2(
						pablohirafuji$elm_qrcode$QRCode$Encode$UTF8$encodeHelp,
						remainStr,
						A2(
							elm$core$List$cons,
							128 | pablohirafuji$elm_qrcode$QRCode$Encode$UTF8$and63(charCode),
							A2(
								elm$core$List$cons,
								128 | pablohirafuji$elm_qrcode$QRCode$Encode$UTF8$and63(charCode >> 6),
								A2(elm$core$List$cons, 224 | (charCode >> 12), list))));
				} else {
					if (remainStr.b) {
						var _char = remainStr.a;
						var strTail = remainStr.b;
						var nextCharCode = elm$core$Char$toCode(_char);
						var charC = 65536 + ((1023 & nextCharCode) | ((1023 & charCode) << 10));
						var byte4 = 128 | pablohirafuji$elm_qrcode$QRCode$Encode$UTF8$and63(charC);
						var byte3 = 128 | pablohirafuji$elm_qrcode$QRCode$Encode$UTF8$and63(charC >> 6);
						var byte2 = 128 | pablohirafuji$elm_qrcode$QRCode$Encode$UTF8$and63(charC >> 12);
						var byte1 = 240 | (charC >> 18);
						return A2(
							pablohirafuji$elm_qrcode$QRCode$Encode$UTF8$encodeHelp,
							strTail,
							A2(
								elm$core$List$cons,
								byte4,
								A2(
									elm$core$List$cons,
									byte3,
									A2(
										elm$core$List$cons,
										byte2,
										A2(elm$core$List$cons, byte1, list)))));
					} else {
						return elm$core$Result$Err(pablohirafuji$elm_qrcode$QRCode$Error$InvalidUTF8Char);
					}
				}
			}
		}
	});
var pablohirafuji$elm_qrcode$QRCode$Encode$UTF8$encode = function (str) {
	return A2(
		elm$core$Result$map,
		elm$core$List$map(
			function (a) {
				return _Utils_Tuple2(a, 8);
			}),
		A2(
			pablohirafuji$elm_qrcode$QRCode$Encode$UTF8$encodeHelp,
			elm$core$String$toList(str),
			_List_Nil));
};
var pablohirafuji$elm_qrcode$QRCode$Encode$encoder = function (mode) {
	switch (mode.$) {
		case 'Numeric':
			return pablohirafuji$elm_qrcode$QRCode$Encode$Numeric$encode;
		case 'Alphanumeric':
			return pablohirafuji$elm_qrcode$QRCode$Encode$Alphanumeric$encode;
		case 'Byte':
			return pablohirafuji$elm_qrcode$QRCode$Encode$Byte$encode;
		default:
			return pablohirafuji$elm_qrcode$QRCode$Encode$UTF8$encode;
	}
};
var pablohirafuji$elm_qrcode$QRCode$ErrorCorrection$expTable = elm$core$Array$fromList(
	_List_fromArray(
		[1, 2, 4, 8, 16, 32, 64, 128, 29, 58, 116, 232, 205, 135, 19, 38, 76, 152, 45, 90, 180, 117, 234, 201, 143, 3, 6, 12, 24, 48, 96, 192, 157, 39, 78, 156, 37, 74, 148, 53, 106, 212, 181, 119, 238, 193, 159, 35, 70, 140, 5, 10, 20, 40, 80, 160, 93, 186, 105, 210, 185, 111, 222, 161, 95, 190, 97, 194, 153, 47, 94, 188, 101, 202, 137, 15, 30, 60, 120, 240, 253, 231, 211, 187, 107, 214, 177, 127, 254, 225, 223, 163, 91, 182, 113, 226, 217, 175, 67, 134, 17, 34, 68, 136, 13, 26, 52, 104, 208, 189, 103, 206, 129, 31, 62, 124, 248, 237, 199, 147, 59, 118, 236, 197, 151, 51, 102, 204, 133, 23, 46, 92, 184, 109, 218, 169, 79, 158, 33, 66, 132, 21, 42, 84, 168, 77, 154, 41, 82, 164, 85, 170, 73, 146, 57, 114, 228, 213, 183, 115, 230, 209, 191, 99, 198, 145, 63, 126, 252, 229, 215, 179, 123, 246, 241, 255, 227, 219, 171, 75, 150, 49, 98, 196, 149, 55, 110, 220, 165, 87, 174, 65, 130, 25, 50, 100, 200, 141, 7, 14, 28, 56, 112, 224, 221, 167, 83, 166, 81, 162, 89, 178, 121, 242, 249, 239, 195, 155, 43, 86, 172, 69, 138, 9, 18, 36, 72, 144, 61, 122, 244, 245, 247, 243, 251, 235, 203, 139, 11, 22, 44, 88, 176, 125, 250, 233, 207, 131, 27, 54, 108, 216, 173, 71, 142, 1]));
var pablohirafuji$elm_qrcode$QRCode$ErrorCorrection$getExp = function (index) {
	return A2(
		elm$core$Maybe$withDefault,
		0,
		A2(
			elm$core$Array$get,
			A2(elm$core$Basics$modBy, 255, index),
			pablohirafuji$elm_qrcode$QRCode$ErrorCorrection$expTable));
};
var elm$core$Array$length = function (_n0) {
	var len = _n0.a;
	return len;
};
var elm$core$Array$setHelp = F4(
	function (shift, index, value, tree) {
		var pos = elm$core$Array$bitMask & (index >>> shift);
		var _n0 = A2(elm$core$Elm$JsArray$unsafeGet, pos, tree);
		if (_n0.$ === 'SubTree') {
			var subTree = _n0.a;
			var newSub = A4(elm$core$Array$setHelp, shift - elm$core$Array$shiftStep, index, value, subTree);
			return A3(
				elm$core$Elm$JsArray$unsafeSet,
				pos,
				elm$core$Array$SubTree(newSub),
				tree);
		} else {
			var values = _n0.a;
			var newLeaf = A3(elm$core$Elm$JsArray$unsafeSet, elm$core$Array$bitMask & index, value, values);
			return A3(
				elm$core$Elm$JsArray$unsafeSet,
				pos,
				elm$core$Array$Leaf(newLeaf),
				tree);
		}
	});
var elm$core$Array$set = F3(
	function (index, value, array) {
		var len = array.a;
		var startShift = array.b;
		var tree = array.c;
		var tail = array.d;
		return ((index < 0) || (_Utils_cmp(index, len) > -1)) ? array : ((_Utils_cmp(
			index,
			elm$core$Array$tailIndex(len)) > -1) ? A4(
			elm$core$Array$Array_elm_builtin,
			len,
			startShift,
			tree,
			A3(elm$core$Elm$JsArray$unsafeSet, elm$core$Array$bitMask & index, value, tail)) : A4(
			elm$core$Array$Array_elm_builtin,
			len,
			startShift,
			A4(elm$core$Array$setHelp, startShift, index, value, tree),
			tail));
	});
var elm$core$Basics$always = F2(
	function (a, _n0) {
		return a;
	});
var pablohirafuji$elm_qrcode$QRCode$Error$PolynomialMultiplyException = {$: 'PolynomialMultiplyException'};
var pablohirafuji$elm_qrcode$QRCode$Error$LogTableException = function (a) {
	return {$: 'LogTableException', a: a};
};
var pablohirafuji$elm_qrcode$QRCode$ErrorCorrection$logTable = elm$core$Array$fromList(
	_List_fromArray(
		[0, 1, 25, 2, 50, 26, 198, 3, 223, 51, 238, 27, 104, 199, 75, 4, 100, 224, 14, 52, 141, 239, 129, 28, 193, 105, 248, 200, 8, 76, 113, 5, 138, 101, 47, 225, 36, 15, 33, 53, 147, 142, 218, 240, 18, 130, 69, 29, 181, 194, 125, 106, 39, 249, 185, 201, 154, 9, 120, 77, 228, 114, 166, 6, 191, 139, 98, 102, 221, 48, 253, 226, 152, 37, 179, 16, 145, 34, 136, 54, 208, 148, 206, 143, 150, 219, 189, 241, 210, 19, 92, 131, 56, 70, 64, 30, 66, 182, 163, 195, 72, 126, 110, 107, 58, 40, 84, 250, 133, 186, 61, 202, 94, 155, 159, 10, 21, 121, 43, 78, 212, 229, 172, 115, 243, 167, 87, 7, 112, 192, 247, 140, 128, 99, 13, 103, 74, 222, 237, 49, 197, 254, 24, 227, 165, 153, 119, 38, 184, 180, 124, 17, 68, 146, 217, 35, 32, 137, 46, 55, 63, 209, 91, 149, 188, 207, 205, 144, 135, 151, 178, 220, 252, 190, 97, 242, 86, 211, 171, 20, 42, 93, 158, 132, 60, 57, 83, 71, 109, 65, 162, 31, 45, 67, 216, 183, 123, 164, 118, 196, 23, 73, 236, 127, 12, 111, 246, 108, 161, 59, 82, 41, 157, 85, 170, 251, 96, 134, 177, 187, 204, 62, 90, 203, 89, 95, 176, 156, 169, 160, 81, 11, 245, 22, 235, 122, 117, 44, 215, 79, 174, 213, 233, 230, 231, 173, 232, 116, 214, 244, 234, 168, 80, 88, 175]));
var pablohirafuji$elm_qrcode$QRCode$ErrorCorrection$getLog = function (index) {
	return (index < 1) ? elm$core$Result$Err(
		pablohirafuji$elm_qrcode$QRCode$Error$LogTableException(index)) : A2(
		elm$core$Result$fromMaybe,
		pablohirafuji$elm_qrcode$QRCode$Error$LogTableException(index),
		A2(elm$core$Array$get, index - 1, pablohirafuji$elm_qrcode$QRCode$ErrorCorrection$logTable));
};
var pablohirafuji$elm_qrcode$QRCode$ErrorCorrection$getOffset = function (_n0) {
	getOffset:
	while (true) {
		var num = _n0.a;
		var offset = _n0.b;
		if (num.b) {
			var head = num.a;
			var tail = num.b;
			if (!head) {
				var $temp$_n0 = _Utils_Tuple2(tail, offset + 1);
				_n0 = $temp$_n0;
				continue getOffset;
			} else {
				return offset;
			}
		} else {
			return offset;
		}
	}
};
var pablohirafuji$elm_qrcode$QRCode$ErrorCorrection$newPolynomial = F2(
	function (num, shift) {
		var offset = pablohirafuji$elm_qrcode$QRCode$ErrorCorrection$getOffset(
			_Utils_Tuple2(num, 0));
		var numArray = elm$core$Array$fromList(num);
		return A2(
			elm$core$Array$initialize,
			(elm$core$List$length(num) - offset) + shift,
			function (index) {
				return A2(
					elm$core$Maybe$withDefault,
					0,
					A2(elm$core$Array$get, index + offset, numArray));
			});
	});
var pablohirafuji$elm_qrcode$QRCode$ErrorCorrection$multiply = F2(
	function (poly1, poly2) {
		var valuesArray = A2(
			elm$core$List$indexedMap,
			F2(
				function (index1, value1) {
					return A2(
						elm$core$List$indexedMap,
						F2(
							function (index2, value2) {
								return _Utils_Tuple3(index1 + index2, value1, value2);
							}),
						elm$core$Array$toList(poly2));
				}),
			elm$core$Array$toList(poly1));
		var process__ = F3(
			function (indexSum, num_, exp) {
				return A2(
					elm$core$Result$fromMaybe,
					pablohirafuji$elm_qrcode$QRCode$Error$PolynomialMultiplyException,
					A2(
						elm$core$Maybe$map,
						elm$core$Bitwise$xor(exp),
						A2(elm$core$Array$get, indexSum, num_)));
			});
		var process_ = F2(
			function (_n0, num_) {
				var indexSum = _n0.a;
				var value1 = _n0.b;
				var value2 = _n0.c;
				return A2(
					elm$core$Result$map,
					function (r) {
						return A3(elm$core$Array$set, indexSum, r, num_);
					},
					A2(
						elm$core$Result$andThen,
						A2(process__, indexSum, num_),
						A2(
							elm$core$Result$map,
							pablohirafuji$elm_qrcode$QRCode$ErrorCorrection$getExp,
							A3(
								elm$core$Result$map2,
								elm$core$Basics$add,
								pablohirafuji$elm_qrcode$QRCode$ErrorCorrection$getLog(value1),
								pablohirafuji$elm_qrcode$QRCode$ErrorCorrection$getLog(value2)))));
			});
		var process = F2(
			function (args, numResult) {
				return A2(
					elm$core$Result$andThen,
					process_(args),
					numResult);
			});
		var num = A2(
			elm$core$Array$initialize,
			(elm$core$Array$length(poly1) + elm$core$Array$length(poly2)) - 1,
			elm$core$Basics$always(0));
		return A2(
			elm$core$Result$map,
			function (a) {
				return A2(pablohirafuji$elm_qrcode$QRCode$ErrorCorrection$newPolynomial, a, 0);
			},
			A2(
				elm$core$Result$map,
				elm$core$Array$toList,
				A3(
					elm$core$List$foldl,
					process,
					elm$core$Result$Ok(num),
					elm$core$List$concat(valuesArray))));
	});
var pablohirafuji$elm_qrcode$QRCode$ErrorCorrection$getECPolynomial = function (ecLength) {
	var generate = F2(
		function (count, polyResult) {
			generate:
			while (true) {
				if (_Utils_cmp(count, ecLength) < 0) {
					var $temp$count = count + 1,
						$temp$polyResult = A2(
						elm$core$Result$andThen,
						function (a) {
							return A2(
								pablohirafuji$elm_qrcode$QRCode$ErrorCorrection$multiply,
								a,
								A2(
									pablohirafuji$elm_qrcode$QRCode$ErrorCorrection$newPolynomial,
									_List_fromArray(
										[
											1,
											pablohirafuji$elm_qrcode$QRCode$ErrorCorrection$getExp(count)
										]),
									0));
						},
						polyResult);
					count = $temp$count;
					polyResult = $temp$polyResult;
					continue generate;
				} else {
					return polyResult;
				}
			}
		});
	return A2(
		generate,
		0,
		elm$core$Result$Ok(
			A2(
				pablohirafuji$elm_qrcode$QRCode$ErrorCorrection$newPolynomial,
				_List_fromArray(
					[1]),
				0)));
};
var pablohirafuji$elm_qrcode$QRCode$ErrorCorrection$get___ = F2(
	function (ecLength, modPoly) {
		return elm$core$Array$toList(
			A2(
				elm$core$Array$initialize,
				ecLength,
				function (index) {
					var modIndex = (index + elm$core$Array$length(modPoly)) - ecLength;
					return (modIndex >= 0) ? A2(
						elm$core$Maybe$withDefault,
						0,
						A2(elm$core$Array$get, modIndex, modPoly)) : 0;
				}));
	});
var elm$core$Elm$JsArray$indexedMap = _JsArray_indexedMap;
var elm$core$Array$indexedMap = F2(
	function (func, _n0) {
		var len = _n0.a;
		var tree = _n0.c;
		var tail = _n0.d;
		var initialBuilder = {
			nodeList: _List_Nil,
			nodeListSize: 0,
			tail: A3(
				elm$core$Elm$JsArray$indexedMap,
				func,
				elm$core$Array$tailIndex(len),
				tail)
		};
		var helper = F2(
			function (node, builder) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3(elm$core$Elm$JsArray$foldl, helper, builder, subTree);
				} else {
					var leaf = node.a;
					var offset = builder.nodeListSize * elm$core$Array$branchFactor;
					var mappedLeaf = elm$core$Array$Leaf(
						A3(elm$core$Elm$JsArray$indexedMap, func, offset, leaf));
					return {
						nodeList: A2(elm$core$List$cons, mappedLeaf, builder.nodeList),
						nodeListSize: builder.nodeListSize + 1,
						tail: builder.tail
					};
				}
			});
		return A2(
			elm$core$Array$builderToArray,
			true,
			A3(elm$core$Elm$JsArray$foldl, helper, initialBuilder, tree));
	});
var pablohirafuji$elm_qrcode$QRCode$Error$PolynomialModException = {$: 'PolynomialModException'};
var pablohirafuji$elm_qrcode$QRCode$ErrorCorrection$mod = F2(
	function (poly1, poly2) {
		if ((elm$core$Array$length(poly1) - elm$core$Array$length(poly2)) < 0) {
			return elm$core$Result$Ok(poly1);
		} else {
			var helper_ = F3(
				function (index2, poly1_, exp) {
					return A2(
						elm$core$Result$fromMaybe,
						pablohirafuji$elm_qrcode$QRCode$Error$PolynomialModException,
						A2(
							elm$core$Maybe$map,
							elm$core$Bitwise$xor(exp),
							A2(elm$core$Array$get, index2, poly1_)));
				});
			var getHead = function (poly) {
				return A2(
					elm$core$Result$andThen,
					pablohirafuji$elm_qrcode$QRCode$ErrorCorrection$getLog,
					A2(
						elm$core$Result$fromMaybe,
						pablohirafuji$elm_qrcode$QRCode$Error$PolynomialModException,
						A2(elm$core$Array$get, 0, poly)));
			};
			var ratio = A3(
				elm$core$Result$map2,
				elm$core$Basics$sub,
				getHead(poly1),
				getHead(poly2));
			var helper = F2(
				function (_n0, poly1_) {
					var index2 = _n0.a;
					var value2 = _n0.b;
					return A2(
						elm$core$Result$map,
						function (r) {
							return A3(elm$core$Array$set, index2, r, poly1_);
						},
						A2(
							elm$core$Result$andThen,
							A2(helper_, index2, poly1_),
							A2(
								elm$core$Result$map,
								pablohirafuji$elm_qrcode$QRCode$ErrorCorrection$getExp,
								A3(
									elm$core$Result$map2,
									elm$core$Basics$add,
									ratio,
									pablohirafuji$elm_qrcode$QRCode$ErrorCorrection$getLog(value2)))));
				});
			var numFold = F2(
				function (args, poly1Result) {
					return A2(
						elm$core$Result$andThen,
						helper(args),
						poly1Result);
				});
			var numResult = A3(
				elm$core$Array$foldl,
				numFold,
				elm$core$Result$Ok(poly1),
				A2(
					elm$core$Array$indexedMap,
					F2(
						function (a, b) {
							return _Utils_Tuple2(a, b);
						}),
					poly2));
			return A2(
				elm$core$Result$andThen,
				function (a) {
					return A2(pablohirafuji$elm_qrcode$QRCode$ErrorCorrection$mod, a, poly2);
				},
				A2(
					elm$core$Result$map,
					function (a) {
						return A2(pablohirafuji$elm_qrcode$QRCode$ErrorCorrection$newPolynomial, a, 0);
					},
					A2(elm$core$Result$map, elm$core$Array$toList, numResult)));
		}
	});
var pablohirafuji$elm_qrcode$QRCode$ErrorCorrection$get__ = F2(
	function (rsPoly, dataCodewords) {
		return A2(
			elm$core$Result$map,
			pablohirafuji$elm_qrcode$QRCode$ErrorCorrection$get___(
				elm$core$Array$length(rsPoly) - 1),
			A2(
				pablohirafuji$elm_qrcode$QRCode$ErrorCorrection$mod,
				A2(
					pablohirafuji$elm_qrcode$QRCode$ErrorCorrection$newPolynomial,
					dataCodewords,
					elm$core$Array$length(rsPoly) - 1),
				rsPoly));
	});
var pablohirafuji$elm_qrcode$QRCode$Helpers$listResult = F3(
	function (fun, listb, lista) {
		if (lista.b) {
			var head = lista.a;
			var tail = lista.b;
			return A2(
				elm$core$Result$andThen,
				function (a) {
					return A3(pablohirafuji$elm_qrcode$QRCode$Helpers$listResult, fun, a, tail);
				},
				A2(
					elm$core$Result$map,
					function (r) {
						return A2(elm$core$List$cons, r, listb);
					},
					fun(head)));
		} else {
			return elm$core$Result$Ok(
				elm$core$List$reverse(listb));
		}
	});
var pablohirafuji$elm_qrcode$QRCode$ErrorCorrection$get_ = F2(
	function (byteBlocks, rsPoly) {
		return A3(
			pablohirafuji$elm_qrcode$QRCode$Helpers$listResult,
			pablohirafuji$elm_qrcode$QRCode$ErrorCorrection$get__(rsPoly),
			_List_Nil,
			byteBlocks);
	});
var pablohirafuji$elm_qrcode$QRCode$ErrorCorrection$get = F2(
	function (ecPerBlock, byteBlocks) {
		return A2(
			elm$core$Result$andThen,
			pablohirafuji$elm_qrcode$QRCode$ErrorCorrection$get_(byteBlocks),
			pablohirafuji$elm_qrcode$QRCode$ErrorCorrection$getECPolynomial(ecPerBlock));
	});
var pablohirafuji$elm_qrcode$QRCode$Encode$getErrorCorrection = function (_n0) {
	var model = _n0.a;
	var dataBlocks = _n0.b;
	return A2(
		elm$core$Result$map,
		function (c) {
			return _Utils_Tuple3(model, dataBlocks, c);
		},
		A2(pablohirafuji$elm_qrcode$QRCode$ErrorCorrection$get, model.groupInfo.ecPerBlock, dataBlocks));
};
var pablohirafuji$elm_qrcode$QRCode$Encode$Alphanumeric = {$: 'Alphanumeric'};
var pablohirafuji$elm_qrcode$QRCode$Encode$Byte = {$: 'Byte'};
var pablohirafuji$elm_qrcode$QRCode$Encode$Numeric = {$: 'Numeric'};
var elm$regex$Regex$Match = F4(
	function (match, index, number, submatches) {
		return {index: index, match: match, number: number, submatches: submatches};
	});
var elm$regex$Regex$contains = _Regex_contains;
var elm$regex$Regex$fromStringWith = _Regex_fromStringWith;
var pablohirafuji$elm_qrcode$QRCode$Encode$Alphanumeric$onlyAlphanumeric = A2(
	elm$regex$Regex$fromStringWith,
	{caseInsensitive: false, multiline: false},
	'^[0-9A-Z $%*+\\-.\\/:]+$');
var pablohirafuji$elm_qrcode$QRCode$Encode$Alphanumeric$isValid = function (input) {
	return A2(
		elm$core$Maybe$withDefault,
		false,
		A2(
			elm$core$Maybe$map,
			function (r) {
				return A2(elm$regex$Regex$contains, r, input);
			},
			pablohirafuji$elm_qrcode$QRCode$Encode$Alphanumeric$onlyAlphanumeric));
};
var pablohirafuji$elm_qrcode$QRCode$Encode$Byte$only8Bit = A2(
	elm$regex$Regex$fromStringWith,
	{caseInsensitive: false, multiline: false},
	'^[\\u0000-\\u00ff]+$');
var pablohirafuji$elm_qrcode$QRCode$Encode$Byte$isValid = function (input) {
	return A2(
		elm$core$Maybe$withDefault,
		false,
		A2(
			elm$core$Maybe$map,
			function (r) {
				return A2(elm$regex$Regex$contains, r, input);
			},
			pablohirafuji$elm_qrcode$QRCode$Encode$Byte$only8Bit));
};
var pablohirafuji$elm_qrcode$QRCode$Encode$Numeric$onlyNumber = A2(
	elm$regex$Regex$fromStringWith,
	{caseInsensitive: false, multiline: false},
	'^[0-9]+$');
var pablohirafuji$elm_qrcode$QRCode$Encode$Numeric$isValid = function (input) {
	return A2(
		elm$core$Maybe$withDefault,
		false,
		A2(
			elm$core$Maybe$map,
			function (r) {
				return A2(elm$regex$Regex$contains, r, input);
			},
			pablohirafuji$elm_qrcode$QRCode$Encode$Numeric$onlyNumber));
};
var pablohirafuji$elm_qrcode$QRCode$Encode$selectMode = function (input) {
	return pablohirafuji$elm_qrcode$QRCode$Encode$Numeric$isValid(input) ? pablohirafuji$elm_qrcode$QRCode$Encode$Numeric : (pablohirafuji$elm_qrcode$QRCode$Encode$Alphanumeric$isValid(input) ? pablohirafuji$elm_qrcode$QRCode$Encode$Alphanumeric : (pablohirafuji$elm_qrcode$QRCode$Encode$Byte$isValid(input) ? pablohirafuji$elm_qrcode$QRCode$Encode$Byte : pablohirafuji$elm_qrcode$QRCode$Encode$UTF8));
};
var elm$core$Tuple$second = function (_n0) {
	var y = _n0.b;
	return y;
};
var elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2(elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var elm$core$List$sortBy = _List_sortBy;
var pablohirafuji$elm_qrcode$QRCode$Encode$filterCapacity = F3(
	function (mode, dataLength, _n0) {
		var version = _n0.version;
		var capacity = _n0.capacity;
		return _Utils_cmp(
			A2(pablohirafuji$elm_qrcode$QRCode$Encode$charCountIndicatorLength, mode, version) + dataLength,
			capacity) < 1;
	});
var pablohirafuji$elm_qrcode$QRCode$Error$InputLengthOverflow = {$: 'InputLengthOverflow'};
var pablohirafuji$elm_qrcode$QRCode$GroupInfo$blockByteCapacity = function (_n0) {
	var blockCount = _n0.a;
	var bytePerBlock = _n0.b;
	return blockCount * bytePerBlock;
};
var pablohirafuji$elm_qrcode$QRCode$GroupInfo$byteCapacity = F2(
	function (group1, maybeGroup2) {
		if (maybeGroup2.$ === 'Just') {
			var block2 = maybeGroup2.a;
			return pablohirafuji$elm_qrcode$QRCode$GroupInfo$blockByteCapacity(group1) + pablohirafuji$elm_qrcode$QRCode$GroupInfo$blockByteCapacity(block2);
		} else {
			return pablohirafuji$elm_qrcode$QRCode$GroupInfo$blockByteCapacity(group1);
		}
	});
var pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo = F4(
	function (version, ecPerBlock, group1, maybeGroup2) {
		return {
			capacity: A2(pablohirafuji$elm_qrcode$QRCode$GroupInfo$byteCapacity, group1, maybeGroup2) * 8,
			ecPerBlock: ecPerBlock,
			group1: group1,
			maybeGroup2: maybeGroup2,
			version: version
		};
	});
var pablohirafuji$elm_qrcode$QRCode$GroupInfo$dataH = _List_fromArray(
	[
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		1,
		17,
		_Utils_Tuple2(1, 9),
		elm$core$Maybe$Nothing),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		2,
		28,
		_Utils_Tuple2(1, 16),
		elm$core$Maybe$Nothing),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		3,
		22,
		_Utils_Tuple2(2, 13),
		elm$core$Maybe$Nothing),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		4,
		16,
		_Utils_Tuple2(4, 9),
		elm$core$Maybe$Nothing),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		5,
		22,
		_Utils_Tuple2(2, 11),
		elm$core$Maybe$Just(
			_Utils_Tuple2(2, 12))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		6,
		28,
		_Utils_Tuple2(4, 15),
		elm$core$Maybe$Nothing),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		7,
		26,
		_Utils_Tuple2(4, 13),
		elm$core$Maybe$Just(
			_Utils_Tuple2(1, 14))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		8,
		26,
		_Utils_Tuple2(4, 14),
		elm$core$Maybe$Just(
			_Utils_Tuple2(2, 15))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		9,
		24,
		_Utils_Tuple2(4, 12),
		elm$core$Maybe$Just(
			_Utils_Tuple2(4, 13))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		10,
		28,
		_Utils_Tuple2(6, 15),
		elm$core$Maybe$Just(
			_Utils_Tuple2(2, 16))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		11,
		24,
		_Utils_Tuple2(3, 12),
		elm$core$Maybe$Just(
			_Utils_Tuple2(8, 13))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		12,
		28,
		_Utils_Tuple2(7, 14),
		elm$core$Maybe$Just(
			_Utils_Tuple2(4, 15))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		13,
		22,
		_Utils_Tuple2(12, 11),
		elm$core$Maybe$Just(
			_Utils_Tuple2(4, 12))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		14,
		24,
		_Utils_Tuple2(11, 12),
		elm$core$Maybe$Just(
			_Utils_Tuple2(5, 13))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		15,
		24,
		_Utils_Tuple2(11, 12),
		elm$core$Maybe$Just(
			_Utils_Tuple2(7, 13))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		16,
		30,
		_Utils_Tuple2(3, 15),
		elm$core$Maybe$Just(
			_Utils_Tuple2(13, 16))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		17,
		28,
		_Utils_Tuple2(2, 14),
		elm$core$Maybe$Just(
			_Utils_Tuple2(17, 15))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		18,
		28,
		_Utils_Tuple2(2, 14),
		elm$core$Maybe$Just(
			_Utils_Tuple2(19, 15))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		19,
		26,
		_Utils_Tuple2(9, 13),
		elm$core$Maybe$Just(
			_Utils_Tuple2(16, 14))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		20,
		28,
		_Utils_Tuple2(15, 15),
		elm$core$Maybe$Just(
			_Utils_Tuple2(10, 16))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		21,
		30,
		_Utils_Tuple2(19, 16),
		elm$core$Maybe$Just(
			_Utils_Tuple2(6, 17))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		22,
		24,
		_Utils_Tuple2(34, 13),
		elm$core$Maybe$Nothing),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		23,
		30,
		_Utils_Tuple2(16, 15),
		elm$core$Maybe$Just(
			_Utils_Tuple2(14, 16))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		24,
		30,
		_Utils_Tuple2(30, 16),
		elm$core$Maybe$Just(
			_Utils_Tuple2(2, 17))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		25,
		30,
		_Utils_Tuple2(22, 15),
		elm$core$Maybe$Just(
			_Utils_Tuple2(13, 16))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		26,
		30,
		_Utils_Tuple2(33, 16),
		elm$core$Maybe$Just(
			_Utils_Tuple2(4, 17))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		27,
		30,
		_Utils_Tuple2(12, 15),
		elm$core$Maybe$Just(
			_Utils_Tuple2(28, 16))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		28,
		30,
		_Utils_Tuple2(11, 15),
		elm$core$Maybe$Just(
			_Utils_Tuple2(31, 16))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		29,
		30,
		_Utils_Tuple2(19, 15),
		elm$core$Maybe$Just(
			_Utils_Tuple2(26, 16))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		30,
		30,
		_Utils_Tuple2(23, 15),
		elm$core$Maybe$Just(
			_Utils_Tuple2(25, 16))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		31,
		30,
		_Utils_Tuple2(23, 15),
		elm$core$Maybe$Just(
			_Utils_Tuple2(28, 16))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		32,
		30,
		_Utils_Tuple2(19, 15),
		elm$core$Maybe$Just(
			_Utils_Tuple2(35, 16))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		33,
		30,
		_Utils_Tuple2(11, 15),
		elm$core$Maybe$Just(
			_Utils_Tuple2(46, 16))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		34,
		30,
		_Utils_Tuple2(59, 16),
		elm$core$Maybe$Just(
			_Utils_Tuple2(1, 17))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		35,
		30,
		_Utils_Tuple2(22, 15),
		elm$core$Maybe$Just(
			_Utils_Tuple2(41, 16))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		36,
		30,
		_Utils_Tuple2(2, 15),
		elm$core$Maybe$Just(
			_Utils_Tuple2(64, 16))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		37,
		30,
		_Utils_Tuple2(24, 15),
		elm$core$Maybe$Just(
			_Utils_Tuple2(46, 16))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		38,
		30,
		_Utils_Tuple2(42, 15),
		elm$core$Maybe$Just(
			_Utils_Tuple2(32, 16))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		39,
		30,
		_Utils_Tuple2(10, 15),
		elm$core$Maybe$Just(
			_Utils_Tuple2(67, 16))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		40,
		30,
		_Utils_Tuple2(20, 15),
		elm$core$Maybe$Just(
			_Utils_Tuple2(61, 16)))
	]);
var pablohirafuji$elm_qrcode$QRCode$GroupInfo$dataL = _List_fromArray(
	[
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		1,
		7,
		_Utils_Tuple2(1, 19),
		elm$core$Maybe$Nothing),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		2,
		10,
		_Utils_Tuple2(1, 34),
		elm$core$Maybe$Nothing),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		3,
		15,
		_Utils_Tuple2(1, 55),
		elm$core$Maybe$Nothing),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		4,
		20,
		_Utils_Tuple2(1, 80),
		elm$core$Maybe$Nothing),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		5,
		26,
		_Utils_Tuple2(1, 108),
		elm$core$Maybe$Nothing),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		6,
		18,
		_Utils_Tuple2(2, 68),
		elm$core$Maybe$Nothing),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		7,
		20,
		_Utils_Tuple2(2, 78),
		elm$core$Maybe$Nothing),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		8,
		24,
		_Utils_Tuple2(2, 97),
		elm$core$Maybe$Nothing),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		9,
		30,
		_Utils_Tuple2(2, 116),
		elm$core$Maybe$Nothing),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		10,
		18,
		_Utils_Tuple2(2, 68),
		elm$core$Maybe$Just(
			_Utils_Tuple2(2, 69))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		11,
		20,
		_Utils_Tuple2(4, 81),
		elm$core$Maybe$Nothing),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		12,
		24,
		_Utils_Tuple2(2, 92),
		elm$core$Maybe$Just(
			_Utils_Tuple2(2, 93))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		13,
		26,
		_Utils_Tuple2(4, 107),
		elm$core$Maybe$Nothing),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		14,
		30,
		_Utils_Tuple2(3, 115),
		elm$core$Maybe$Just(
			_Utils_Tuple2(1, 116))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		15,
		22,
		_Utils_Tuple2(5, 87),
		elm$core$Maybe$Just(
			_Utils_Tuple2(1, 88))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		16,
		24,
		_Utils_Tuple2(5, 98),
		elm$core$Maybe$Just(
			_Utils_Tuple2(1, 99))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		17,
		28,
		_Utils_Tuple2(1, 107),
		elm$core$Maybe$Just(
			_Utils_Tuple2(5, 108))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		18,
		30,
		_Utils_Tuple2(5, 120),
		elm$core$Maybe$Just(
			_Utils_Tuple2(1, 121))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		19,
		28,
		_Utils_Tuple2(3, 113),
		elm$core$Maybe$Just(
			_Utils_Tuple2(4, 114))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		20,
		28,
		_Utils_Tuple2(3, 107),
		elm$core$Maybe$Just(
			_Utils_Tuple2(5, 108))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		21,
		28,
		_Utils_Tuple2(4, 116),
		elm$core$Maybe$Just(
			_Utils_Tuple2(4, 117))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		22,
		28,
		_Utils_Tuple2(2, 111),
		elm$core$Maybe$Just(
			_Utils_Tuple2(7, 112))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		23,
		30,
		_Utils_Tuple2(4, 121),
		elm$core$Maybe$Just(
			_Utils_Tuple2(5, 122))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		24,
		30,
		_Utils_Tuple2(6, 117),
		elm$core$Maybe$Just(
			_Utils_Tuple2(4, 118))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		25,
		26,
		_Utils_Tuple2(8, 106),
		elm$core$Maybe$Just(
			_Utils_Tuple2(4, 107))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		26,
		28,
		_Utils_Tuple2(10, 114),
		elm$core$Maybe$Just(
			_Utils_Tuple2(2, 115))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		27,
		30,
		_Utils_Tuple2(8, 122),
		elm$core$Maybe$Just(
			_Utils_Tuple2(4, 123))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		28,
		30,
		_Utils_Tuple2(3, 117),
		elm$core$Maybe$Just(
			_Utils_Tuple2(10, 118))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		29,
		30,
		_Utils_Tuple2(7, 116),
		elm$core$Maybe$Just(
			_Utils_Tuple2(7, 117))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		30,
		30,
		_Utils_Tuple2(5, 115),
		elm$core$Maybe$Just(
			_Utils_Tuple2(10, 116))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		31,
		30,
		_Utils_Tuple2(13, 115),
		elm$core$Maybe$Just(
			_Utils_Tuple2(3, 116))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		32,
		30,
		_Utils_Tuple2(17, 115),
		elm$core$Maybe$Nothing),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		33,
		30,
		_Utils_Tuple2(17, 115),
		elm$core$Maybe$Just(
			_Utils_Tuple2(1, 116))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		34,
		30,
		_Utils_Tuple2(13, 115),
		elm$core$Maybe$Just(
			_Utils_Tuple2(6, 116))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		35,
		30,
		_Utils_Tuple2(12, 121),
		elm$core$Maybe$Just(
			_Utils_Tuple2(7, 122))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		36,
		30,
		_Utils_Tuple2(6, 121),
		elm$core$Maybe$Just(
			_Utils_Tuple2(14, 122))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		37,
		30,
		_Utils_Tuple2(17, 122),
		elm$core$Maybe$Just(
			_Utils_Tuple2(4, 123))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		38,
		30,
		_Utils_Tuple2(4, 122),
		elm$core$Maybe$Just(
			_Utils_Tuple2(18, 123))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		39,
		30,
		_Utils_Tuple2(20, 117),
		elm$core$Maybe$Just(
			_Utils_Tuple2(4, 118))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		40,
		30,
		_Utils_Tuple2(19, 118),
		elm$core$Maybe$Just(
			_Utils_Tuple2(6, 119)))
	]);
var pablohirafuji$elm_qrcode$QRCode$GroupInfo$dataM = _List_fromArray(
	[
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		1,
		10,
		_Utils_Tuple2(1, 16),
		elm$core$Maybe$Nothing),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		2,
		16,
		_Utils_Tuple2(1, 28),
		elm$core$Maybe$Nothing),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		3,
		26,
		_Utils_Tuple2(1, 44),
		elm$core$Maybe$Nothing),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		4,
		18,
		_Utils_Tuple2(2, 32),
		elm$core$Maybe$Nothing),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		5,
		24,
		_Utils_Tuple2(2, 43),
		elm$core$Maybe$Nothing),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		6,
		16,
		_Utils_Tuple2(4, 27),
		elm$core$Maybe$Nothing),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		7,
		18,
		_Utils_Tuple2(4, 31),
		elm$core$Maybe$Nothing),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		8,
		22,
		_Utils_Tuple2(2, 38),
		elm$core$Maybe$Just(
			_Utils_Tuple2(2, 39))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		9,
		22,
		_Utils_Tuple2(3, 36),
		elm$core$Maybe$Just(
			_Utils_Tuple2(2, 37))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		10,
		26,
		_Utils_Tuple2(4, 43),
		elm$core$Maybe$Just(
			_Utils_Tuple2(1, 44))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		11,
		30,
		_Utils_Tuple2(1, 50),
		elm$core$Maybe$Just(
			_Utils_Tuple2(4, 51))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		12,
		22,
		_Utils_Tuple2(6, 36),
		elm$core$Maybe$Just(
			_Utils_Tuple2(2, 37))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		13,
		22,
		_Utils_Tuple2(8, 37),
		elm$core$Maybe$Just(
			_Utils_Tuple2(1, 38))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		14,
		24,
		_Utils_Tuple2(4, 40),
		elm$core$Maybe$Just(
			_Utils_Tuple2(5, 41))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		15,
		24,
		_Utils_Tuple2(5, 41),
		elm$core$Maybe$Just(
			_Utils_Tuple2(5, 42))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		16,
		28,
		_Utils_Tuple2(7, 45),
		elm$core$Maybe$Just(
			_Utils_Tuple2(3, 46))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		17,
		28,
		_Utils_Tuple2(10, 46),
		elm$core$Maybe$Just(
			_Utils_Tuple2(1, 47))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		18,
		26,
		_Utils_Tuple2(9, 43),
		elm$core$Maybe$Just(
			_Utils_Tuple2(4, 44))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		19,
		26,
		_Utils_Tuple2(3, 44),
		elm$core$Maybe$Just(
			_Utils_Tuple2(11, 45))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		20,
		26,
		_Utils_Tuple2(3, 41),
		elm$core$Maybe$Just(
			_Utils_Tuple2(13, 42))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		21,
		26,
		_Utils_Tuple2(17, 42),
		elm$core$Maybe$Nothing),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		22,
		28,
		_Utils_Tuple2(17, 46),
		elm$core$Maybe$Nothing),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		23,
		28,
		_Utils_Tuple2(4, 47),
		elm$core$Maybe$Just(
			_Utils_Tuple2(14, 48))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		24,
		28,
		_Utils_Tuple2(6, 45),
		elm$core$Maybe$Just(
			_Utils_Tuple2(14, 46))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		25,
		28,
		_Utils_Tuple2(8, 47),
		elm$core$Maybe$Just(
			_Utils_Tuple2(13, 48))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		26,
		28,
		_Utils_Tuple2(19, 46),
		elm$core$Maybe$Just(
			_Utils_Tuple2(4, 47))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		27,
		28,
		_Utils_Tuple2(22, 45),
		elm$core$Maybe$Just(
			_Utils_Tuple2(3, 46))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		28,
		28,
		_Utils_Tuple2(3, 45),
		elm$core$Maybe$Just(
			_Utils_Tuple2(23, 46))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		29,
		28,
		_Utils_Tuple2(21, 45),
		elm$core$Maybe$Just(
			_Utils_Tuple2(7, 46))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		30,
		28,
		_Utils_Tuple2(19, 47),
		elm$core$Maybe$Just(
			_Utils_Tuple2(10, 48))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		31,
		28,
		_Utils_Tuple2(2, 46),
		elm$core$Maybe$Just(
			_Utils_Tuple2(29, 47))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		32,
		28,
		_Utils_Tuple2(10, 46),
		elm$core$Maybe$Just(
			_Utils_Tuple2(23, 47))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		33,
		28,
		_Utils_Tuple2(14, 46),
		elm$core$Maybe$Just(
			_Utils_Tuple2(21, 47))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		34,
		28,
		_Utils_Tuple2(14, 46),
		elm$core$Maybe$Just(
			_Utils_Tuple2(23, 47))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		35,
		28,
		_Utils_Tuple2(12, 47),
		elm$core$Maybe$Just(
			_Utils_Tuple2(26, 48))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		36,
		28,
		_Utils_Tuple2(6, 47),
		elm$core$Maybe$Just(
			_Utils_Tuple2(34, 48))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		37,
		28,
		_Utils_Tuple2(29, 46),
		elm$core$Maybe$Just(
			_Utils_Tuple2(14, 47))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		38,
		28,
		_Utils_Tuple2(13, 46),
		elm$core$Maybe$Just(
			_Utils_Tuple2(32, 47))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		39,
		28,
		_Utils_Tuple2(40, 47),
		elm$core$Maybe$Just(
			_Utils_Tuple2(7, 48))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		40,
		28,
		_Utils_Tuple2(18, 47),
		elm$core$Maybe$Just(
			_Utils_Tuple2(31, 48)))
	]);
var pablohirafuji$elm_qrcode$QRCode$GroupInfo$dataQ = _List_fromArray(
	[
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		1,
		13,
		_Utils_Tuple2(1, 13),
		elm$core$Maybe$Nothing),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		2,
		22,
		_Utils_Tuple2(1, 22),
		elm$core$Maybe$Nothing),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		3,
		18,
		_Utils_Tuple2(2, 17),
		elm$core$Maybe$Nothing),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		4,
		26,
		_Utils_Tuple2(2, 24),
		elm$core$Maybe$Nothing),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		5,
		18,
		_Utils_Tuple2(2, 15),
		elm$core$Maybe$Just(
			_Utils_Tuple2(2, 16))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		6,
		24,
		_Utils_Tuple2(4, 19),
		elm$core$Maybe$Nothing),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		7,
		18,
		_Utils_Tuple2(2, 14),
		elm$core$Maybe$Just(
			_Utils_Tuple2(4, 15))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		8,
		22,
		_Utils_Tuple2(4, 18),
		elm$core$Maybe$Just(
			_Utils_Tuple2(2, 19))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		9,
		20,
		_Utils_Tuple2(4, 16),
		elm$core$Maybe$Just(
			_Utils_Tuple2(4, 17))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		10,
		24,
		_Utils_Tuple2(6, 19),
		elm$core$Maybe$Just(
			_Utils_Tuple2(2, 20))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		11,
		28,
		_Utils_Tuple2(4, 22),
		elm$core$Maybe$Just(
			_Utils_Tuple2(4, 23))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		12,
		26,
		_Utils_Tuple2(4, 20),
		elm$core$Maybe$Just(
			_Utils_Tuple2(6, 21))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		13,
		24,
		_Utils_Tuple2(8, 20),
		elm$core$Maybe$Just(
			_Utils_Tuple2(4, 21))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		14,
		20,
		_Utils_Tuple2(11, 16),
		elm$core$Maybe$Just(
			_Utils_Tuple2(5, 17))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		15,
		30,
		_Utils_Tuple2(5, 24),
		elm$core$Maybe$Just(
			_Utils_Tuple2(7, 25))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		16,
		24,
		_Utils_Tuple2(15, 19),
		elm$core$Maybe$Just(
			_Utils_Tuple2(2, 20))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		17,
		28,
		_Utils_Tuple2(1, 22),
		elm$core$Maybe$Just(
			_Utils_Tuple2(15, 23))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		18,
		28,
		_Utils_Tuple2(17, 22),
		elm$core$Maybe$Just(
			_Utils_Tuple2(1, 23))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		19,
		26,
		_Utils_Tuple2(17, 21),
		elm$core$Maybe$Just(
			_Utils_Tuple2(4, 22))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		20,
		30,
		_Utils_Tuple2(15, 24),
		elm$core$Maybe$Just(
			_Utils_Tuple2(5, 25))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		21,
		28,
		_Utils_Tuple2(17, 22),
		elm$core$Maybe$Just(
			_Utils_Tuple2(6, 23))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		22,
		30,
		_Utils_Tuple2(7, 24),
		elm$core$Maybe$Just(
			_Utils_Tuple2(16, 25))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		23,
		30,
		_Utils_Tuple2(11, 24),
		elm$core$Maybe$Just(
			_Utils_Tuple2(14, 25))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		24,
		30,
		_Utils_Tuple2(11, 24),
		elm$core$Maybe$Just(
			_Utils_Tuple2(16, 25))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		25,
		30,
		_Utils_Tuple2(7, 24),
		elm$core$Maybe$Just(
			_Utils_Tuple2(22, 25))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		26,
		28,
		_Utils_Tuple2(28, 22),
		elm$core$Maybe$Just(
			_Utils_Tuple2(6, 23))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		27,
		30,
		_Utils_Tuple2(8, 23),
		elm$core$Maybe$Just(
			_Utils_Tuple2(26, 24))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		28,
		30,
		_Utils_Tuple2(4, 24),
		elm$core$Maybe$Just(
			_Utils_Tuple2(31, 25))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		29,
		30,
		_Utils_Tuple2(1, 23),
		elm$core$Maybe$Just(
			_Utils_Tuple2(37, 24))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		30,
		30,
		_Utils_Tuple2(15, 24),
		elm$core$Maybe$Just(
			_Utils_Tuple2(25, 25))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		31,
		30,
		_Utils_Tuple2(42, 24),
		elm$core$Maybe$Just(
			_Utils_Tuple2(1, 25))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		32,
		30,
		_Utils_Tuple2(10, 24),
		elm$core$Maybe$Just(
			_Utils_Tuple2(35, 25))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		33,
		30,
		_Utils_Tuple2(29, 24),
		elm$core$Maybe$Just(
			_Utils_Tuple2(19, 25))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		34,
		30,
		_Utils_Tuple2(44, 24),
		elm$core$Maybe$Just(
			_Utils_Tuple2(7, 25))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		35,
		30,
		_Utils_Tuple2(39, 24),
		elm$core$Maybe$Just(
			_Utils_Tuple2(14, 25))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		36,
		30,
		_Utils_Tuple2(46, 24),
		elm$core$Maybe$Just(
			_Utils_Tuple2(10, 25))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		37,
		30,
		_Utils_Tuple2(49, 24),
		elm$core$Maybe$Just(
			_Utils_Tuple2(10, 25))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		38,
		30,
		_Utils_Tuple2(48, 24),
		elm$core$Maybe$Just(
			_Utils_Tuple2(14, 25))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		39,
		30,
		_Utils_Tuple2(43, 24),
		elm$core$Maybe$Just(
			_Utils_Tuple2(22, 25))),
		A4(
		pablohirafuji$elm_qrcode$QRCode$GroupInfo$newGroupInfo,
		40,
		30,
		_Utils_Tuple2(34, 24),
		elm$core$Maybe$Just(
			_Utils_Tuple2(34, 25)))
	]);
var pablohirafuji$elm_qrcode$QRCode$GroupInfo$getGroupData = function (ecLevel) {
	switch (ecLevel.$) {
		case 'L':
			return pablohirafuji$elm_qrcode$QRCode$GroupInfo$dataL;
		case 'M':
			return pablohirafuji$elm_qrcode$QRCode$GroupInfo$dataM;
		case 'Q':
			return pablohirafuji$elm_qrcode$QRCode$GroupInfo$dataQ;
		default:
			return pablohirafuji$elm_qrcode$QRCode$GroupInfo$dataH;
	}
};
var pablohirafuji$elm_qrcode$QRCode$Encode$getVersion = F3(
	function (ecLevel, mode, dataLength) {
		return A2(
			elm$core$Result$fromMaybe,
			pablohirafuji$elm_qrcode$QRCode$Error$InputLengthOverflow,
			elm$core$List$head(
				A2(
					elm$core$List$sortBy,
					function ($) {
						return $.capacity;
					},
					A2(
						elm$core$List$filter,
						A2(pablohirafuji$elm_qrcode$QRCode$Encode$filterCapacity, mode, dataLength),
						pablohirafuji$elm_qrcode$QRCode$GroupInfo$getGroupData(ecLevel)))));
	});
var pablohirafuji$elm_qrcode$QRCode$Encode$versionToModel = F5(
	function (inputStr, ecLevel, mode, partialBitsCount, groupInfo) {
		return {
			bitsCount: partialBitsCount + A2(pablohirafuji$elm_qrcode$QRCode$Encode$charCountIndicatorLength, mode, groupInfo.version),
			ecLevel: ecLevel,
			groupInfo: groupInfo,
			inputStr: inputStr,
			mode: mode
		};
	});
var pablohirafuji$elm_qrcode$QRCode$Encode$selectVersion = F4(
	function (inputStr, ecLevel, mode, encodedStr) {
		var partialBitsCount = 4 + A3(
			elm$core$List$foldl,
			F2(
				function (a, b) {
					return a.b + b;
				}),
			0,
			encodedStr);
		return A2(
			elm$core$Result$map,
			function (b) {
				return _Utils_Tuple2(encodedStr, b);
			},
			A2(
				elm$core$Result$map,
				A4(pablohirafuji$elm_qrcode$QRCode$Encode$versionToModel, inputStr, ecLevel, mode, partialBitsCount),
				A3(pablohirafuji$elm_qrcode$QRCode$Encode$getVersion, ecLevel, mode, partialBitsCount)));
	});
var pablohirafuji$elm_qrcode$QRCode$Encode$breakList = F3(
	function (checkFinish, _n0, _n1) {
		breakList:
		while (true) {
			var times = _n0.a;
			var itemCount = _n0.b;
			var byteList = _n1.a;
			var progress = _n1.b;
			if (times > 0) {
				var remainList = A2(elm$core$List$drop, itemCount, byteList);
				var block = A2(elm$core$List$take, itemCount, byteList);
				var $temp$checkFinish = checkFinish,
					$temp$_n0 = _Utils_Tuple2(times - 1, itemCount),
					$temp$_n1 = _Utils_Tuple2(
					remainList,
					A2(elm$core$List$cons, block, progress));
				checkFinish = $temp$checkFinish;
				_n0 = $temp$_n0;
				_n1 = $temp$_n1;
				continue breakList;
			} else {
				if (checkFinish && (elm$core$List$length(byteList) > 0)) {
					return elm$core$Result$Err(pablohirafuji$elm_qrcode$QRCode$Error$InputLengthOverflow);
				} else {
					return elm$core$Result$Ok(
						_Utils_Tuple2(byteList, progress));
				}
			}
		}
	});
var pablohirafuji$elm_qrcode$QRCode$Encode$toBlocks = function (_n0) {
	var model = _n0.a;
	var groupInfo = model.groupInfo;
	var byteList = _n0.b;
	var _n1 = groupInfo.maybeGroup2;
	if (_n1.$ === 'Just') {
		var group2 = _n1.a;
		return A2(
			elm$core$Result$map,
			function (b) {
				return _Utils_Tuple2(model, b);
			},
			A2(
				elm$core$Result$map,
				A2(elm$core$Basics$composeR, elm$core$Tuple$second, elm$core$List$reverse),
				A2(
					elm$core$Result$andThen,
					A2(pablohirafuji$elm_qrcode$QRCode$Encode$breakList, true, group2),
					A3(
						pablohirafuji$elm_qrcode$QRCode$Encode$breakList,
						false,
						groupInfo.group1,
						_Utils_Tuple2(byteList, _List_Nil)))));
	} else {
		return A2(
			elm$core$Result$map,
			function (b) {
				return _Utils_Tuple2(model, b);
			},
			A2(
				elm$core$Result$map,
				A2(elm$core$Basics$composeR, elm$core$Tuple$second, elm$core$List$reverse),
				A3(
					pablohirafuji$elm_qrcode$QRCode$Encode$breakList,
					true,
					groupInfo.group1,
					_Utils_Tuple2(byteList, _List_Nil))));
	}
};
var pablohirafuji$elm_qrcode$QRCode$Encode$encode = F2(
	function (inputStr, ecLevel) {
		var mode = pablohirafuji$elm_qrcode$QRCode$Encode$selectMode(inputStr);
		return A2(
			elm$core$Result$map,
			pablohirafuji$elm_qrcode$QRCode$Encode$concatTranspose,
			A2(
				elm$core$Result$andThen,
				pablohirafuji$elm_qrcode$QRCode$Encode$getErrorCorrection,
				A2(
					elm$core$Result$andThen,
					pablohirafuji$elm_qrcode$QRCode$Encode$toBlocks,
					A2(
						elm$core$Result$map,
						pablohirafuji$elm_qrcode$QRCode$Encode$addInfoAndFinalBits,
						A2(
							elm$core$Result$andThen,
							A3(pablohirafuji$elm_qrcode$QRCode$Encode$selectVersion, inputStr, ecLevel, mode),
							A2(pablohirafuji$elm_qrcode$QRCode$Encode$encoder, mode, inputStr))))));
	});
var elm$core$Basics$negate = function (n) {
	return -n;
};
var pablohirafuji$elm_qrcode$QRCode$Matrix$getIndex = F3(
	function (size, row, col) {
		return (size * row) + col;
	});
var pablohirafuji$elm_qrcode$QRCode$Matrix$isOccupy = F4(
	function (row, col, size, matrix) {
		var _n0 = A2(
			elm$core$Array$get,
			A3(pablohirafuji$elm_qrcode$QRCode$Matrix$getIndex, size, row, col),
			matrix);
		if ((_n0.$ === 'Just') && (_n0.a.$ === 'Just')) {
			return true;
		} else {
			return false;
		}
	});
var pablohirafuji$elm_qrcode$QRCode$Matrix$nextModule = function (placement) {
	var row = placement.row;
	var col = placement.col;
	var isRight = placement.isRight;
	var isUp = placement.isUp;
	return isRight ? _Utils_update(
		placement,
		{col: col - 1, isRight: false}) : (isUp ? _Utils_update(
		placement,
		{col: col + 1, isRight: true, row: row - 1}) : _Utils_update(
		placement,
		{col: col + 1, isRight: true, row: row + 1}));
};
var pablohirafuji$elm_qrcode$QRCode$Matrix$bitToColor = F2(
	function (_byte, offset) {
		return (1 & (_byte >> (7 - offset))) === 1;
	});
var pablohirafuji$elm_qrcode$QRCode$Matrix$setDataModule = F3(
	function (_n0, _byte, offset) {
		var size = _n0.size;
		var row = _n0.row;
		var col = _n0.col;
		return A2(
			elm$core$Array$set,
			A3(pablohirafuji$elm_qrcode$QRCode$Matrix$getIndex, size, row, col),
			elm$core$Maybe$Just(
				_Utils_Tuple2(
					false,
					A2(pablohirafuji$elm_qrcode$QRCode$Matrix$bitToColor, _byte, offset))));
	});
var pablohirafuji$elm_qrcode$QRCode$Matrix$addDataModule = F4(
	function (placement, bytes, offset, matrix) {
		addDataModule:
		while (true) {
			var size = placement.size;
			var row = placement.row;
			var col = placement.col;
			if (!bytes.b) {
				return matrix;
			} else {
				var head = bytes.a;
				var tail = bytes.b;
				if (offset >= 8) {
					var $temp$placement = placement,
						$temp$bytes = tail,
						$temp$offset = 0,
						$temp$matrix = matrix;
					placement = $temp$placement;
					bytes = $temp$bytes;
					offset = $temp$offset;
					matrix = $temp$matrix;
					continue addDataModule;
				} else {
					if (col === 6) {
						var $temp$placement = _Utils_update(
							placement,
							{col: col - 1, isRight: true}),
							$temp$bytes = bytes,
							$temp$offset = offset,
							$temp$matrix = matrix;
						placement = $temp$placement;
						bytes = $temp$bytes;
						offset = $temp$offset;
						matrix = $temp$matrix;
						continue addDataModule;
					} else {
						if (row < 0) {
							var $temp$placement = _Utils_update(
								placement,
								{col: col - 2, isRight: true, isUp: false, row: 0}),
								$temp$bytes = bytes,
								$temp$offset = offset,
								$temp$matrix = matrix;
							placement = $temp$placement;
							bytes = $temp$bytes;
							offset = $temp$offset;
							matrix = $temp$matrix;
							continue addDataModule;
						} else {
							if (_Utils_cmp(row, size) > -1) {
								var $temp$placement = _Utils_update(
									placement,
									{col: col - 2, isRight: true, isUp: true, row: size - 1}),
									$temp$bytes = bytes,
									$temp$offset = offset,
									$temp$matrix = matrix;
								placement = $temp$placement;
								bytes = $temp$bytes;
								offset = $temp$offset;
								matrix = $temp$matrix;
								continue addDataModule;
							} else {
								if (A4(pablohirafuji$elm_qrcode$QRCode$Matrix$isOccupy, row, col, size, matrix)) {
									var $temp$placement = pablohirafuji$elm_qrcode$QRCode$Matrix$nextModule(placement),
										$temp$bytes = bytes,
										$temp$offset = offset,
										$temp$matrix = matrix;
									placement = $temp$placement;
									bytes = $temp$bytes;
									offset = $temp$offset;
									matrix = $temp$matrix;
									continue addDataModule;
								} else {
									var $temp$placement = pablohirafuji$elm_qrcode$QRCode$Matrix$nextModule(placement),
										$temp$bytes = bytes,
										$temp$offset = offset + 1,
										$temp$matrix = A4(pablohirafuji$elm_qrcode$QRCode$Matrix$setDataModule, placement, head, offset, matrix);
									placement = $temp$placement;
									bytes = $temp$bytes;
									offset = $temp$offset;
									matrix = $temp$matrix;
									continue addDataModule;
								}
							}
						}
					}
				}
			}
		}
	});
var pablohirafuji$elm_qrcode$QRCode$Matrix$initPlacement = function (size) {
	return {col: size + 1, isRight: true, isUp: true, row: size + 1, size: size};
};
var pablohirafuji$elm_qrcode$QRCode$Matrix$addData = F3(
	function (size, bytes, matrix) {
		return A4(
			pablohirafuji$elm_qrcode$QRCode$Matrix$addDataModule,
			pablohirafuji$elm_qrcode$QRCode$Matrix$initPlacement(size),
			bytes,
			0,
			matrix);
	});
var pablohirafuji$elm_qrcode$QRCode$Error$AlignmentPatternNotFound = {$: 'AlignmentPatternNotFound'};
var pablohirafuji$elm_qrcode$QRCode$Matrix$alignmentPatternData = elm$core$Array$fromList(
	_List_fromArray(
		[
			_List_Nil,
			_List_fromArray(
			[6, 18]),
			_List_fromArray(
			[6, 22]),
			_List_fromArray(
			[6, 26]),
			_List_fromArray(
			[6, 30]),
			_List_fromArray(
			[6, 34]),
			_List_fromArray(
			[6, 22, 38]),
			_List_fromArray(
			[6, 24, 42]),
			_List_fromArray(
			[6, 26, 46]),
			_List_fromArray(
			[6, 28, 50]),
			_List_fromArray(
			[6, 30, 54]),
			_List_fromArray(
			[6, 32, 58]),
			_List_fromArray(
			[6, 34, 62]),
			_List_fromArray(
			[6, 26, 46, 66]),
			_List_fromArray(
			[6, 26, 48, 70]),
			_List_fromArray(
			[6, 26, 50, 74]),
			_List_fromArray(
			[6, 30, 54, 78]),
			_List_fromArray(
			[6, 30, 56, 82]),
			_List_fromArray(
			[6, 30, 58, 86]),
			_List_fromArray(
			[6, 34, 62, 90]),
			_List_fromArray(
			[6, 28, 50, 72, 94]),
			_List_fromArray(
			[6, 26, 50, 74, 98]),
			_List_fromArray(
			[6, 30, 54, 78, 102]),
			_List_fromArray(
			[6, 28, 54, 80, 106]),
			_List_fromArray(
			[6, 32, 58, 84, 110]),
			_List_fromArray(
			[6, 30, 58, 86, 114]),
			_List_fromArray(
			[6, 34, 62, 90, 118]),
			_List_fromArray(
			[6, 26, 50, 74, 98, 122]),
			_List_fromArray(
			[6, 30, 54, 78, 102, 126]),
			_List_fromArray(
			[6, 26, 52, 78, 104, 130]),
			_List_fromArray(
			[6, 30, 56, 82, 108, 134]),
			_List_fromArray(
			[6, 34, 60, 86, 112, 138]),
			_List_fromArray(
			[6, 30, 58, 86, 114, 142]),
			_List_fromArray(
			[6, 34, 62, 90, 118, 146]),
			_List_fromArray(
			[6, 30, 54, 78, 102, 126, 150]),
			_List_fromArray(
			[6, 24, 50, 76, 102, 128, 154]),
			_List_fromArray(
			[6, 28, 54, 80, 106, 132, 158]),
			_List_fromArray(
			[6, 32, 58, 84, 110, 136, 162]),
			_List_fromArray(
			[6, 26, 54, 82, 110, 138, 166]),
			_List_fromArray(
			[6, 30, 58, 86, 114, 142, 170])
		]));
var pablohirafuji$elm_qrcode$QRCode$Matrix$getAreaCoord = F2(
	function (rows, cols) {
		return A3(
			elm$core$List$foldl,
			F2(
				function (row, list) {
					return A3(
						elm$core$List$foldl,
						F2(
							function (col, list_) {
								return A2(
									elm$core$List$cons,
									_Utils_Tuple2(row, col),
									list_);
							}),
						list,
						cols);
				}),
			_List_Nil,
			rows);
	});
var pablohirafuji$elm_qrcode$QRCode$Matrix$isValidAlign = F2(
	function (size, _n0) {
		var row = _n0.a;
		var col = _n0.b;
		return ((row > 10) || ((10 < col) && (_Utils_cmp(col, size - 10) < 0))) && ((_Utils_cmp(row, size - 10) < 0) || (col > 10));
	});
var pablohirafuji$elm_qrcode$QRCode$Matrix$alignmentRange = A2(elm$core$List$range, -2, 2);
var pablohirafuji$elm_qrcode$QRCode$Matrix$alignmentColor = F2(
	function (row, col) {
		return (_Utils_eq(row, -2) || ((row === 2) || (_Utils_eq(col, -2) || ((col === 2) || ((!row) && (!col)))))) ? elm$core$Maybe$Just(
			_Utils_Tuple2(true, true)) : elm$core$Maybe$Just(
			_Utils_Tuple2(true, false));
	});
var pablohirafuji$elm_qrcode$QRCode$Matrix$setAlignModule = F4(
	function (size, rowPos, colPos, _n0) {
		var row = _n0.a;
		var col = _n0.b;
		return A2(
			elm$core$Array$set,
			A3(pablohirafuji$elm_qrcode$QRCode$Matrix$getIndex, size, row + rowPos, col + colPos),
			A2(pablohirafuji$elm_qrcode$QRCode$Matrix$alignmentColor, row, col));
	});
var pablohirafuji$elm_qrcode$QRCode$Matrix$setAlignment = F3(
	function (size, _n0, matrix) {
		var row = _n0.a;
		var col = _n0.b;
		return A3(
			elm$core$List$foldl,
			A3(pablohirafuji$elm_qrcode$QRCode$Matrix$setAlignModule, size, row, col),
			matrix,
			A2(pablohirafuji$elm_qrcode$QRCode$Matrix$getAreaCoord, pablohirafuji$elm_qrcode$QRCode$Matrix$alignmentRange, pablohirafuji$elm_qrcode$QRCode$Matrix$alignmentRange));
	});
var pablohirafuji$elm_qrcode$QRCode$Matrix$setAlignments = F3(
	function (size, locations, matrix) {
		return A3(
			elm$core$List$foldl,
			pablohirafuji$elm_qrcode$QRCode$Matrix$setAlignment(size),
			matrix,
			A2(
				elm$core$List$filter,
				pablohirafuji$elm_qrcode$QRCode$Matrix$isValidAlign(size),
				A2(pablohirafuji$elm_qrcode$QRCode$Matrix$getAreaCoord, locations, locations)));
	});
var pablohirafuji$elm_qrcode$QRCode$Matrix$alignmentPattern = F3(
	function (version, size, matrix) {
		return A2(
			elm$core$Result$map,
			function (a) {
				return A3(pablohirafuji$elm_qrcode$QRCode$Matrix$setAlignments, size, a, matrix);
			},
			A2(
				elm$core$Result$fromMaybe,
				pablohirafuji$elm_qrcode$QRCode$Error$AlignmentPatternNotFound,
				A2(elm$core$Array$get, version - 1, pablohirafuji$elm_qrcode$QRCode$Matrix$alignmentPatternData)));
	});
var pablohirafuji$elm_qrcode$QRCode$Matrix$darkModule = F2(
	function (version, size) {
		return A2(
			elm$core$Array$set,
			A3(pablohirafuji$elm_qrcode$QRCode$Matrix$getIndex, size, (4 * version) + 9, 8),
			elm$core$Maybe$Just(
				_Utils_Tuple2(true, true)));
	});
var pablohirafuji$elm_qrcode$QRCode$Matrix$finderRange = A2(elm$core$List$range, 0, 8);
var pablohirafuji$elm_qrcode$QRCode$Matrix$finderColor = F2(
	function (row, col) {
		return ((1 <= row) && ((row <= 7) && ((col === 1) || (col === 7)))) || (((1 <= col) && ((col <= 7) && ((row === 1) || (row === 7)))) || ((3 <= row) && ((row <= 5) && ((3 <= col) && (col <= 5)))));
	});
var pablohirafuji$elm_qrcode$QRCode$Matrix$setFinder = F5(
	function (size, rowOffset, colOffset, _n0, matrix) {
		var row = _n0.a;
		var col = _n0.b;
		var finalRow = row + rowOffset;
		var finalCol = col + colOffset;
		return ((finalRow < 0) || ((finalCol < 0) || ((_Utils_cmp(finalRow, size) > -1) || (_Utils_cmp(finalCol, size) > -1)))) ? matrix : A3(
			elm$core$Array$set,
			A3(pablohirafuji$elm_qrcode$QRCode$Matrix$getIndex, size, finalRow, finalCol),
			elm$core$Maybe$Just(
				_Utils_Tuple2(
					true,
					A2(pablohirafuji$elm_qrcode$QRCode$Matrix$finderColor, row, col))),
			matrix);
	});
var pablohirafuji$elm_qrcode$QRCode$Matrix$finderPattern = F4(
	function (size, rowOffset, colOffset, matrix) {
		return A3(
			elm$core$List$foldl,
			A3(pablohirafuji$elm_qrcode$QRCode$Matrix$setFinder, size, rowOffset, colOffset),
			matrix,
			A2(pablohirafuji$elm_qrcode$QRCode$Matrix$getAreaCoord, pablohirafuji$elm_qrcode$QRCode$Matrix$finderRange, pablohirafuji$elm_qrcode$QRCode$Matrix$finderRange));
	});
var elm$core$Basics$not = _Basics_not;
var pablohirafuji$elm_qrcode$QRCode$Matrix$applyMaskColor = F2(
	function (maybeModule, isChange) {
		if (isChange) {
			if ((maybeModule.$ === 'Just') && (!maybeModule.a.a)) {
				var _n1 = maybeModule.a;
				var isDark = _n1.b;
				return elm$core$Maybe$Just(
					_Utils_Tuple2(false, !isDark));
			} else {
				return maybeModule;
			}
		} else {
			return maybeModule;
		}
	});
var pablohirafuji$elm_qrcode$QRCode$Matrix$getCoord = F2(
	function (size, index) {
		return _Utils_Tuple2(
			(index / size) | 0,
			A2(elm$core$Basics$modBy, size, index));
	});
var pablohirafuji$elm_qrcode$QRCode$Matrix$applyMaskFunction = F4(
	function (_function, size, index, maybeModule) {
		return A2(
			pablohirafuji$elm_qrcode$QRCode$Matrix$applyMaskColor,
			maybeModule,
			_function(
				A2(pablohirafuji$elm_qrcode$QRCode$Matrix$getCoord, size, index)));
	});
var pablohirafuji$elm_qrcode$QRCode$Matrix$maskFunction = function (mask) {
	switch (mask.$) {
		case 'Pattern0':
			return function (_n1) {
				var row = _n1.a;
				var col = _n1.b;
				return !A2(elm$core$Basics$modBy, 2, row + col);
			};
		case 'Pattern1':
			return function (_n2) {
				var row = _n2.a;
				return !A2(elm$core$Basics$modBy, 2, row);
			};
		case 'Pattern2':
			return function (_n3) {
				var col = _n3.b;
				return !A2(elm$core$Basics$modBy, 3, col);
			};
		case 'Pattern3':
			return function (_n4) {
				var row = _n4.a;
				var col = _n4.b;
				return !A2(elm$core$Basics$modBy, 3, row + col);
			};
		case 'Pattern4':
			return function (_n5) {
				var row = _n5.a;
				var col = _n5.b;
				return !A2(
					elm$core$Basics$modBy,
					2,
					elm$core$Basics$floor(row / 2) + elm$core$Basics$floor(col / 3));
			};
		case 'Pattern5':
			return function (_n6) {
				var row = _n6.a;
				var col = _n6.b;
				return !(A2(elm$core$Basics$modBy, 2, row * col) + A2(elm$core$Basics$modBy, 3, row * col));
			};
		case 'Pattern6':
			return function (_n7) {
				var row = _n7.a;
				var col = _n7.b;
				return !A2(
					elm$core$Basics$modBy,
					2,
					A2(elm$core$Basics$modBy, 2, row * col) + A2(elm$core$Basics$modBy, 3, row * col));
			};
		default:
			return function (_n8) {
				var row = _n8.a;
				var col = _n8.b;
				return !A2(
					elm$core$Basics$modBy,
					2,
					A2(elm$core$Basics$modBy, 3, row * col) + A2(elm$core$Basics$modBy, 2, row + col));
			};
	}
};
var pablohirafuji$elm_qrcode$QRCode$Matrix$applyMask = F3(
	function (size, mask, matrix) {
		return A2(
			elm$core$Array$indexedMap,
			A2(
				pablohirafuji$elm_qrcode$QRCode$Matrix$applyMaskFunction,
				pablohirafuji$elm_qrcode$QRCode$Matrix$maskFunction(mask),
				size),
			matrix);
	});
var pablohirafuji$elm_qrcode$QRCode$Matrix$breakList = F3(
	function (width, list, acc) {
		breakList:
		while (true) {
			if (!list.b) {
				return elm$core$List$reverse(acc);
			} else {
				var $temp$width = width,
					$temp$list = A2(elm$core$List$drop, width, list),
					$temp$acc = A2(
					elm$core$List$cons,
					A2(elm$core$List$take, width, list),
					acc);
				width = $temp$width;
				list = $temp$list;
				acc = $temp$acc;
				continue breakList;
			}
		}
	});
var pablohirafuji$elm_qrcode$QRCode$Matrix$isDarkModule = A2(
	elm$core$Basics$composeR,
	elm$core$Maybe$map(elm$core$Tuple$second),
	elm$core$Maybe$withDefault(false));
var elm$core$List$sum = function (numbers) {
	return A3(elm$core$List$foldl, elm$core$Basics$add, 0, numbers);
};
var pablohirafuji$elm_qrcode$QRCode$Matrix$rule1Score_ = F2(
	function (simplifiedList, _n0) {
		rule1Score_:
		while (true) {
			var last = _n0.a;
			var partialScore = _n0.b;
			var score = _n0.c;
			if (!simplifiedList.b) {
				return (partialScore >= 5) ? ((score + partialScore) - 2) : score;
			} else {
				var head = simplifiedList.a;
				var tail = simplifiedList.b;
				if (_Utils_eq(last, head)) {
					var $temp$simplifiedList = tail,
						$temp$_n0 = _Utils_Tuple3(last, partialScore + 1, score);
					simplifiedList = $temp$simplifiedList;
					_n0 = $temp$_n0;
					continue rule1Score_;
				} else {
					if (partialScore >= 5) {
						var $temp$simplifiedList = tail,
							$temp$_n0 = _Utils_Tuple3(head, 0, (score + partialScore) - 2);
						simplifiedList = $temp$simplifiedList;
						_n0 = $temp$_n0;
						continue rule1Score_;
					} else {
						var $temp$simplifiedList = tail,
							$temp$_n0 = _Utils_Tuple3(head, 0, score);
						simplifiedList = $temp$simplifiedList;
						_n0 = $temp$_n0;
						continue rule1Score_;
					}
				}
			}
		}
	});
var pablohirafuji$elm_qrcode$QRCode$Matrix$rule1Score = A2(
	elm$core$Basics$composeR,
	elm$core$List$map(
		function (a) {
			return A2(
				pablohirafuji$elm_qrcode$QRCode$Matrix$rule1Score_,
				a,
				_Utils_Tuple3(false, 0, 0));
		}),
	elm$core$List$sum);
var pablohirafuji$elm_qrcode$QRCode$Matrix$rule2Score_ = F4(
	function (row1, row2, maybeLast, score) {
		rule2Score_:
		while (true) {
			if (!row1.b) {
				return score;
			} else {
				var head = row1.a;
				var tail = row1.b;
				if (!row2.b) {
					return score;
				} else {
					var head2 = row2.a;
					var tail2 = row2.b;
					if (_Utils_eq(head, head2)) {
						if (_Utils_eq(
							elm$core$Maybe$Just(head),
							maybeLast)) {
							var $temp$row1 = tail,
								$temp$row2 = tail2,
								$temp$maybeLast = elm$core$Maybe$Just(head),
								$temp$score = score + 3;
							row1 = $temp$row1;
							row2 = $temp$row2;
							maybeLast = $temp$maybeLast;
							score = $temp$score;
							continue rule2Score_;
						} else {
							var $temp$row1 = tail,
								$temp$row2 = tail2,
								$temp$maybeLast = elm$core$Maybe$Just(head),
								$temp$score = score;
							row1 = $temp$row1;
							row2 = $temp$row2;
							maybeLast = $temp$maybeLast;
							score = $temp$score;
							continue rule2Score_;
						}
					} else {
						var $temp$row1 = tail,
							$temp$row2 = tail2,
							$temp$maybeLast = elm$core$Maybe$Nothing,
							$temp$score = score;
						row1 = $temp$row1;
						row2 = $temp$row2;
						maybeLast = $temp$maybeLast;
						score = $temp$score;
						continue rule2Score_;
					}
				}
			}
		}
	});
var pablohirafuji$elm_qrcode$QRCode$Matrix$rule2Score = F2(
	function (list, score) {
		rule2Score:
		while (true) {
			if (list.b && list.b.b) {
				var head1 = list.a;
				var _n1 = list.b;
				var head2 = _n1.a;
				var tail = _n1.b;
				var $temp$list = tail,
					$temp$score = score + A4(pablohirafuji$elm_qrcode$QRCode$Matrix$rule2Score_, head1, head2, elm$core$Maybe$Nothing, 0);
				list = $temp$list;
				score = $temp$score;
				continue rule2Score;
			} else {
				return score;
			}
		}
	});
var pablohirafuji$elm_qrcode$QRCode$Matrix$rule3Score_ = F2(
	function (simplifiedList, score) {
		rule3Score_:
		while (true) {
			_n0$3:
			while (true) {
				if (!simplifiedList.b) {
					return score;
				} else {
					if (!simplifiedList.a) {
						if (((((((((((((((((((simplifiedList.b.b && (!simplifiedList.b.a)) && simplifiedList.b.b.b) && (!simplifiedList.b.b.a)) && simplifiedList.b.b.b.b) && (!simplifiedList.b.b.b.a)) && simplifiedList.b.b.b.b.b) && simplifiedList.b.b.b.b.a) && simplifiedList.b.b.b.b.b.b) && (!simplifiedList.b.b.b.b.b.a)) && simplifiedList.b.b.b.b.b.b.b) && simplifiedList.b.b.b.b.b.b.a) && simplifiedList.b.b.b.b.b.b.b.b) && simplifiedList.b.b.b.b.b.b.b.a) && simplifiedList.b.b.b.b.b.b.b.b.b) && simplifiedList.b.b.b.b.b.b.b.b.a) && simplifiedList.b.b.b.b.b.b.b.b.b.b) && (!simplifiedList.b.b.b.b.b.b.b.b.b.a)) && simplifiedList.b.b.b.b.b.b.b.b.b.b.b) && simplifiedList.b.b.b.b.b.b.b.b.b.b.a) {
							var _n1 = simplifiedList.b;
							var _n2 = _n1.b;
							var _n3 = _n2.b;
							var _n4 = _n3.b;
							var _n5 = _n4.b;
							var _n6 = _n5.b;
							var _n7 = _n6.b;
							var _n8 = _n7.b;
							var _n9 = _n8.b;
							var _n10 = _n9.b;
							var tail = _n10.b;
							var $temp$simplifiedList = tail,
								$temp$score = score + 40;
							simplifiedList = $temp$simplifiedList;
							score = $temp$score;
							continue rule3Score_;
						} else {
							break _n0$3;
						}
					} else {
						if (((((((((((((((((((simplifiedList.b.b && (!simplifiedList.b.a)) && simplifiedList.b.b.b) && simplifiedList.b.b.a) && simplifiedList.b.b.b.b) && simplifiedList.b.b.b.a) && simplifiedList.b.b.b.b.b) && simplifiedList.b.b.b.b.a) && simplifiedList.b.b.b.b.b.b) && (!simplifiedList.b.b.b.b.b.a)) && simplifiedList.b.b.b.b.b.b.b) && simplifiedList.b.b.b.b.b.b.a) && simplifiedList.b.b.b.b.b.b.b.b) && (!simplifiedList.b.b.b.b.b.b.b.a)) && simplifiedList.b.b.b.b.b.b.b.b.b) && (!simplifiedList.b.b.b.b.b.b.b.b.a)) && simplifiedList.b.b.b.b.b.b.b.b.b.b) && (!simplifiedList.b.b.b.b.b.b.b.b.b.a)) && simplifiedList.b.b.b.b.b.b.b.b.b.b.b) && (!simplifiedList.b.b.b.b.b.b.b.b.b.b.a)) {
							var _n11 = simplifiedList.b;
							var _n12 = _n11.b;
							var _n13 = _n12.b;
							var _n14 = _n13.b;
							var _n15 = _n14.b;
							var _n16 = _n15.b;
							var _n17 = _n16.b;
							var _n18 = _n17.b;
							var _n19 = _n18.b;
							var _n20 = _n19.b;
							var tail = _n20.b;
							var $temp$simplifiedList = tail,
								$temp$score = score + 40;
							simplifiedList = $temp$simplifiedList;
							score = $temp$score;
							continue rule3Score_;
						} else {
							break _n0$3;
						}
					}
				}
			}
			var head = simplifiedList.a;
			var tail = simplifiedList.b;
			var $temp$simplifiedList = tail,
				$temp$score = score;
			simplifiedList = $temp$simplifiedList;
			score = $temp$score;
			continue rule3Score_;
		}
	});
var pablohirafuji$elm_qrcode$QRCode$Matrix$rule3Score = A2(elm$core$List$foldl, pablohirafuji$elm_qrcode$QRCode$Matrix$rule3Score_, 0);
var elm$core$Basics$abs = function (n) {
	return (n < 0) ? (-n) : n;
};
var elm$core$Basics$round = _Basics_round;
var pablohirafuji$elm_qrcode$QRCode$Matrix$rule4Score = F2(
	function (size, simplifiedList) {
		var moduleCount = size * size;
		var darkCount = elm$core$List$length(
			A2(elm$core$List$filter, elm$core$Basics$identity, simplifiedList));
		var darkPerc = elm$core$Basics$round((100 * darkCount) / moduleCount);
		var remOf5 = darkPerc % 5;
		var nextMult5 = elm$core$Basics$round(
			elm$core$Basics$abs((darkPerc + (5 - remOf5)) - 50) / 5);
		var prevMult5 = elm$core$Basics$round(
			elm$core$Basics$abs((darkPerc - remOf5) - 50) / 5);
		return A2(elm$core$Basics$min, prevMult5, nextMult5) * 10;
	});
var pablohirafuji$elm_qrcode$QRCode$Matrix$getMaskScore = F2(
	function (size, matrix) {
		var list = A2(
			elm$core$List$map,
			pablohirafuji$elm_qrcode$QRCode$Matrix$isDarkModule,
			elm$core$Array$toList(matrix));
		var rowList = A3(pablohirafuji$elm_qrcode$QRCode$Matrix$breakList, size, list, _List_Nil);
		var transposedRowList = pablohirafuji$elm_qrcode$QRCode$Helpers$transpose(rowList);
		return function (b) {
			return _Utils_Tuple2(rowList, b);
		}(
			A2(pablohirafuji$elm_qrcode$QRCode$Matrix$rule4Score, size, list) + (pablohirafuji$elm_qrcode$QRCode$Matrix$rule3Score(transposedRowList) + (pablohirafuji$elm_qrcode$QRCode$Matrix$rule3Score(rowList) + (A2(pablohirafuji$elm_qrcode$QRCode$Matrix$rule2Score, rowList, 0) + (pablohirafuji$elm_qrcode$QRCode$Matrix$rule1Score(transposedRowList) + pablohirafuji$elm_qrcode$QRCode$Matrix$rule1Score(rowList))))));
	});
var pablohirafuji$elm_qrcode$QRCode$Matrix$ecLevelToInt = function (ecLevel) {
	switch (ecLevel.$) {
		case 'L':
			return 1;
		case 'M':
			return 0;
		case 'Q':
			return 3;
		default:
			return 2;
	}
};
var pablohirafuji$elm_qrcode$QRCode$Matrix$getBCHDigit = function (_int) {
	var helper = F2(
		function (digit, int_) {
			helper:
			while (true) {
				if (int_) {
					var $temp$digit = digit + 1,
						$temp$int_ = int_ >>> 1;
					digit = $temp$digit;
					int_ = $temp$int_;
					continue helper;
				} else {
					return digit;
				}
			}
		});
	return A2(helper, 0, _int);
};
var pablohirafuji$elm_qrcode$QRCode$Matrix$maskToInt = function (mask) {
	switch (mask.$) {
		case 'Pattern0':
			return 0;
		case 'Pattern1':
			return 1;
		case 'Pattern2':
			return 2;
		case 'Pattern3':
			return 3;
		case 'Pattern4':
			return 4;
		case 'Pattern5':
			return 5;
		case 'Pattern6':
			return 6;
		default:
			return 7;
	}
};
var pablohirafuji$elm_qrcode$QRCode$Matrix$encodeFormatInfo = F2(
	function (ecLevel, mask) {
		var g15Mask = 21522;
		var g15Int = 1335;
		var g15Digit = pablohirafuji$elm_qrcode$QRCode$Matrix$getBCHDigit(g15Int);
		var formatInfoInt = pablohirafuji$elm_qrcode$QRCode$Matrix$maskToInt(mask) | (pablohirafuji$elm_qrcode$QRCode$Matrix$ecLevelToInt(ecLevel) << 3);
		var helper = function (d_) {
			helper:
			while (true) {
				if ((pablohirafuji$elm_qrcode$QRCode$Matrix$getBCHDigit(d_) - g15Digit) >= 0) {
					var $temp$d_ = d_ ^ (g15Int << (pablohirafuji$elm_qrcode$QRCode$Matrix$getBCHDigit(d_) - g15Digit));
					d_ = $temp$d_;
					continue helper;
				} else {
					return g15Mask ^ (d_ | (formatInfoInt << 10));
				}
			}
		};
		var d = formatInfoInt << 10;
		return helper(d);
	});
var pablohirafuji$elm_qrcode$QRCode$Matrix$formatInfoHorizontal = F2(
	function (size, count) {
		return (count < 8) ? _Utils_Tuple2(8, (size - count) - 1) : ((count < 9) ? _Utils_Tuple2(8, 15 - count) : _Utils_Tuple2(8, (15 - count) - 1));
	});
var pablohirafuji$elm_qrcode$QRCode$Matrix$formatInfoVertical = F2(
	function (size, count) {
		return (count < 6) ? _Utils_Tuple2(count, 8) : ((count < 8) ? _Utils_Tuple2(count + 1, 8) : _Utils_Tuple2((size - 15) + count, 8));
	});
var pablohirafuji$elm_qrcode$QRCode$Matrix$setFormatModule = F4(
	function (size, isBlack, row, col) {
		return A2(
			elm$core$Array$set,
			A3(pablohirafuji$elm_qrcode$QRCode$Matrix$getIndex, size, row, col),
			elm$core$Maybe$Just(
				_Utils_Tuple2(true, isBlack)));
	});
var pablohirafuji$elm_qrcode$QRCode$Matrix$setFormatInfo_ = F4(
	function (size, isBlackFn, count, matrix) {
		setFormatInfo_:
		while (true) {
			if (count < 15) {
				var isBlack = isBlackFn(count);
				var _n0 = A2(pablohirafuji$elm_qrcode$QRCode$Matrix$formatInfoVertical, size, count);
				var x2 = _n0.a;
				var y2 = _n0.b;
				var _n1 = A2(pablohirafuji$elm_qrcode$QRCode$Matrix$formatInfoHorizontal, size, count);
				var x1 = _n1.a;
				var y1 = _n1.b;
				var $temp$size = size,
					$temp$isBlackFn = isBlackFn,
					$temp$count = count + 1,
					$temp$matrix = A5(
					pablohirafuji$elm_qrcode$QRCode$Matrix$setFormatModule,
					size,
					isBlack,
					x2,
					y2,
					A5(pablohirafuji$elm_qrcode$QRCode$Matrix$setFormatModule, size, isBlack, x1, y1, matrix));
				size = $temp$size;
				isBlackFn = $temp$isBlackFn;
				count = $temp$count;
				matrix = $temp$matrix;
				continue setFormatInfo_;
			} else {
				return matrix;
			}
		}
	});
var pablohirafuji$elm_qrcode$QRCode$Matrix$setFormatInfo = F4(
	function (ecLevel, size, mask, matrix) {
		var isBlack = F2(
			function (bits_, count) {
				return (1 & (bits_ >> count)) === 1;
			});
		var bits = A2(pablohirafuji$elm_qrcode$QRCode$Matrix$encodeFormatInfo, ecLevel, mask);
		return A4(
			pablohirafuji$elm_qrcode$QRCode$Matrix$setFormatInfo_,
			size,
			isBlack(bits),
			0,
			matrix);
	});
var pablohirafuji$elm_qrcode$QRCode$Matrix$getBestMask_ = F5(
	function (ecLevel, size, matrix, mask, _n0) {
		var minSMatrix = _n0.a;
		var minScore = _n0.b;
		var maskedMatrix = A4(
			pablohirafuji$elm_qrcode$QRCode$Matrix$setFormatInfo,
			ecLevel,
			size,
			mask,
			A3(pablohirafuji$elm_qrcode$QRCode$Matrix$applyMask, size, mask, matrix));
		var _n1 = A2(pablohirafuji$elm_qrcode$QRCode$Matrix$getMaskScore, size, maskedMatrix);
		var maskSMatrix = _n1.a;
		var maskScore = _n1.b;
		return ((_Utils_cmp(minScore, maskScore) < 0) && (!_Utils_eq(minScore, -1))) ? _Utils_Tuple2(minSMatrix, minScore) : _Utils_Tuple2(maskSMatrix, maskScore);
	});
var pablohirafuji$elm_qrcode$QRCode$Matrix$Pattern0 = {$: 'Pattern0'};
var pablohirafuji$elm_qrcode$QRCode$Matrix$Pattern1 = {$: 'Pattern1'};
var pablohirafuji$elm_qrcode$QRCode$Matrix$Pattern2 = {$: 'Pattern2'};
var pablohirafuji$elm_qrcode$QRCode$Matrix$Pattern3 = {$: 'Pattern3'};
var pablohirafuji$elm_qrcode$QRCode$Matrix$Pattern4 = {$: 'Pattern4'};
var pablohirafuji$elm_qrcode$QRCode$Matrix$Pattern5 = {$: 'Pattern5'};
var pablohirafuji$elm_qrcode$QRCode$Matrix$Pattern6 = {$: 'Pattern6'};
var pablohirafuji$elm_qrcode$QRCode$Matrix$Pattern7 = {$: 'Pattern7'};
var pablohirafuji$elm_qrcode$QRCode$Matrix$patternList = _List_fromArray(
	[pablohirafuji$elm_qrcode$QRCode$Matrix$Pattern0, pablohirafuji$elm_qrcode$QRCode$Matrix$Pattern1, pablohirafuji$elm_qrcode$QRCode$Matrix$Pattern2, pablohirafuji$elm_qrcode$QRCode$Matrix$Pattern3, pablohirafuji$elm_qrcode$QRCode$Matrix$Pattern4, pablohirafuji$elm_qrcode$QRCode$Matrix$Pattern5, pablohirafuji$elm_qrcode$QRCode$Matrix$Pattern6, pablohirafuji$elm_qrcode$QRCode$Matrix$Pattern7]);
var pablohirafuji$elm_qrcode$QRCode$Matrix$getBestMask = F3(
	function (ecLevel, size, matrix) {
		return A3(
			elm$core$List$foldl,
			A3(pablohirafuji$elm_qrcode$QRCode$Matrix$getBestMask_, ecLevel, size, matrix),
			_Utils_Tuple2(_List_Nil, -1),
			pablohirafuji$elm_qrcode$QRCode$Matrix$patternList).a;
	});
var pablohirafuji$elm_qrcode$QRCode$Matrix$reserveFormatInfo = F2(
	function (size, matrix) {
		return A4(
			pablohirafuji$elm_qrcode$QRCode$Matrix$setFormatInfo_,
			size,
			elm$core$Basics$always(true),
			0,
			matrix);
	});
var pablohirafuji$elm_qrcode$QRCode$Matrix$encodeVersionInfo = function (version) {
	var g18Int = 7973;
	var g18Digit = pablohirafuji$elm_qrcode$QRCode$Matrix$getBCHDigit(g18Int);
	var helper = function (d_) {
		helper:
		while (true) {
			if ((pablohirafuji$elm_qrcode$QRCode$Matrix$getBCHDigit(d_) - g18Digit) >= 0) {
				var $temp$d_ = d_ ^ (g18Int << (pablohirafuji$elm_qrcode$QRCode$Matrix$getBCHDigit(d_) - g18Digit));
				d_ = $temp$d_;
				continue helper;
			} else {
				return d_ | (version << 12);
			}
		}
	};
	var d = version << 12;
	return helper(d);
};
var pablohirafuji$elm_qrcode$QRCode$Matrix$setVersionModule = F3(
	function (size, isBlack, _n0) {
		var row = _n0.a;
		var col = _n0.b;
		return A2(
			elm$core$Array$set,
			A3(pablohirafuji$elm_qrcode$QRCode$Matrix$getIndex, size, row, col),
			elm$core$Maybe$Just(
				_Utils_Tuple2(true, isBlack)));
	});
var pablohirafuji$elm_qrcode$QRCode$Matrix$setVersionInfo_ = F4(
	function (size, isBlackFn, count, matrix) {
		setVersionInfo_:
		while (true) {
			if (count < 18) {
				var topRight = _Utils_Tuple2(
					elm$core$Basics$floor(count / 3),
					((A2(elm$core$Basics$modBy, 3, count) + size) - 8) - 3);
				var isBlack = isBlackFn(count);
				var bottomLeft = _Utils_Tuple2(
					((A2(elm$core$Basics$modBy, 3, count) + size) - 8) - 3,
					elm$core$Basics$floor(count / 3));
				var $temp$size = size,
					$temp$isBlackFn = isBlackFn,
					$temp$count = count + 1,
					$temp$matrix = A4(
					pablohirafuji$elm_qrcode$QRCode$Matrix$setVersionModule,
					size,
					isBlack,
					bottomLeft,
					A4(pablohirafuji$elm_qrcode$QRCode$Matrix$setVersionModule, size, isBlack, topRight, matrix));
				size = $temp$size;
				isBlackFn = $temp$isBlackFn;
				count = $temp$count;
				matrix = $temp$matrix;
				continue setVersionInfo_;
			} else {
				return matrix;
			}
		}
	});
var pablohirafuji$elm_qrcode$QRCode$Matrix$setVersionInfo = F3(
	function (version, size, matrix) {
		if (version >= 7) {
			var isBlack = F2(
				function (bits_, count) {
					return (1 & (bits_ >> count)) === 1;
				});
			var bits = pablohirafuji$elm_qrcode$QRCode$Matrix$encodeVersionInfo(version);
			return A4(
				pablohirafuji$elm_qrcode$QRCode$Matrix$setVersionInfo_,
				size,
				isBlack(bits),
				0,
				matrix);
		} else {
			return matrix;
		}
	});
var pablohirafuji$elm_qrcode$QRCode$Matrix$timingColor = F2(
	function (row, col) {
		return (!A2(elm$core$Basics$modBy, 2, row + col)) ? elm$core$Maybe$Just(
			_Utils_Tuple2(true, true)) : elm$core$Maybe$Just(
			_Utils_Tuple2(true, false));
	});
var pablohirafuji$elm_qrcode$QRCode$Matrix$setTiming = F3(
	function (size, row, col) {
		return A2(
			elm$core$Array$set,
			A3(pablohirafuji$elm_qrcode$QRCode$Matrix$getIndex, size, row, col),
			A2(pablohirafuji$elm_qrcode$QRCode$Matrix$timingColor, row, col));
	});
var pablohirafuji$elm_qrcode$QRCode$Matrix$timingPattern = F2(
	function (size, matrix) {
		var range = A2(elm$core$List$range, 8, size - 9);
		return A3(
			elm$core$List$foldl,
			function (b) {
				return A3(pablohirafuji$elm_qrcode$QRCode$Matrix$setTiming, size, b, 6);
			},
			A3(
				elm$core$List$foldl,
				A2(pablohirafuji$elm_qrcode$QRCode$Matrix$setTiming, size, 6),
				matrix,
				range),
			range);
	});
var pablohirafuji$elm_qrcode$QRCode$Matrix$apply = function (_n0) {
	var ecLevel = _n0.a.ecLevel;
	var groupInfo = _n0.a.groupInfo;
	var bytes = _n0.b;
	var version = groupInfo.version;
	var size = ((version - 1) * 4) + 21;
	return A2(
		elm$core$Result$map,
		A2(pablohirafuji$elm_qrcode$QRCode$Matrix$getBestMask, ecLevel, size),
		A2(
			elm$core$Result$map,
			A2(pablohirafuji$elm_qrcode$QRCode$Matrix$addData, size, bytes),
			A3(
				pablohirafuji$elm_qrcode$QRCode$Matrix$alignmentPattern,
				version,
				size,
				A2(
					pablohirafuji$elm_qrcode$QRCode$Matrix$timingPattern,
					size,
					A3(
						pablohirafuji$elm_qrcode$QRCode$Matrix$darkModule,
						version,
						size,
						A3(
							pablohirafuji$elm_qrcode$QRCode$Matrix$setVersionInfo,
							version,
							size,
							A2(
								pablohirafuji$elm_qrcode$QRCode$Matrix$reserveFormatInfo,
								size,
								A4(
									pablohirafuji$elm_qrcode$QRCode$Matrix$finderPattern,
									size,
									-1,
									size - 8,
									A4(
										pablohirafuji$elm_qrcode$QRCode$Matrix$finderPattern,
										size,
										size - 8,
										-1,
										A4(
											pablohirafuji$elm_qrcode$QRCode$Matrix$finderPattern,
											size,
											-1,
											-1,
											A2(
												elm$core$Array$initialize,
												size * size,
												elm$core$Basics$always(elm$core$Maybe$Nothing))))))))))));
};
var pablohirafuji$elm_qrcode$QRCode$encodeWith = F2(
	function (ecLevel, input) {
		return A2(
			elm$core$Result$mapError,
			pablohirafuji$elm_qrcode$QRCode$convertError,
			A2(
				elm$core$Result$map,
				pablohirafuji$elm_qrcode$QRCode$QRCode,
				A2(
					elm$core$Result$andThen,
					pablohirafuji$elm_qrcode$QRCode$Matrix$apply,
					A2(
						pablohirafuji$elm_qrcode$QRCode$Encode$encode,
						input,
						pablohirafuji$elm_qrcode$QRCode$convertEC(ecLevel)))));
	});
var pablohirafuji$elm_qrcode$QRCode$encode = pablohirafuji$elm_qrcode$QRCode$encodeWith(pablohirafuji$elm_qrcode$QRCode$Quartile);
var elm$core$String$concat = function (strings) {
	return A2(elm$core$String$join, '', strings);
};
var elm$core$String$fromFloat = _String_fromNumber;
var elm$svg$Svg$trustedNode = _VirtualDom_nodeNS('http://www.w3.org/2000/svg');
var elm$svg$Svg$path = elm$svg$Svg$trustedNode('path');
var elm$svg$Svg$svg = elm$svg$Svg$trustedNode('svg');
var elm$svg$Svg$Attributes$d = _VirtualDom_attribute('d');
var elm$svg$Svg$Attributes$height = _VirtualDom_attribute('height');
var elm$svg$Svg$Attributes$shapeRendering = _VirtualDom_attribute('shape-rendering');
var elm$svg$Svg$Attributes$stroke = _VirtualDom_attribute('stroke');
var elm$svg$Svg$Attributes$strokeWidth = _VirtualDom_attribute('stroke-width');
var elm$svg$Svg$Attributes$style = _VirtualDom_attribute('style');
var elm$svg$Svg$Attributes$transform = _VirtualDom_attribute('transform');
var elm$svg$Svg$Attributes$viewBox = _VirtualDom_attribute('viewBox');
var elm$svg$Svg$Attributes$width = _VirtualDom_attribute('width');
var pablohirafuji$elm_qrcode$QRCode$Render$Svg$moduleSize = 5;
var pablohirafuji$elm_qrcode$QRCode$Render$Svg$appendLastRect = function (_n0) {
	var lastRect = _n0.a;
	var rowLines = _n0.b;
	return A2(
		elm$core$List$cons,
		'h' + elm$core$String$fromInt(lastRect.width * pablohirafuji$elm_qrcode$QRCode$Render$Svg$moduleSize),
		rowLines);
};
var pablohirafuji$elm_qrcode$QRCode$Render$Svg$toRowLines = F2(
	function (isDark, _n0) {
		var lastRect = _n0.a;
		var rowLines = _n0.b;
		return isDark ? ((!lastRect.space) ? _Utils_Tuple2(
			_Utils_update(
				lastRect,
				{width: lastRect.width + 1}),
			rowLines) : _Utils_Tuple2(
			{space: 0, width: 1},
			A2(
				elm$core$List$cons,
				elm$core$String$concat(
					_List_fromArray(
						[
							(lastRect.width > 0) ? ('h' + elm$core$String$fromInt(lastRect.width * pablohirafuji$elm_qrcode$QRCode$Render$Svg$moduleSize)) : '',
							'm',
							elm$core$String$fromInt(lastRect.space * pablohirafuji$elm_qrcode$QRCode$Render$Svg$moduleSize),
							' 0'
						])),
				rowLines))) : _Utils_Tuple2(
			_Utils_update(
				lastRect,
				{space: lastRect.space + 1}),
			rowLines);
	});
var pablohirafuji$elm_qrcode$QRCode$Render$Svg$viewRow = F3(
	function (quietZoneSize, rowIndex, rowLines) {
		return A2(
			elm$core$List$cons,
			'M0 ',
			A2(
				elm$core$List$cons,
				elm$core$String$fromInt(rowIndex * pablohirafuji$elm_qrcode$QRCode$Render$Svg$moduleSize),
				rowLines));
	});
var pablohirafuji$elm_qrcode$QRCode$Render$Svg$viewBase = F2(
	function (quietZoneSize, matrix) {
		var quietZonePx = quietZoneSize * pablohirafuji$elm_qrcode$QRCode$Render$Svg$moduleSize;
		var sizePx = elm$core$String$fromInt(
			(elm$core$List$length(matrix) * pablohirafuji$elm_qrcode$QRCode$Render$Svg$moduleSize) + (2 * quietZonePx));
		return A2(
			elm$svg$Svg$svg,
			_List_fromArray(
				[
					elm$svg$Svg$Attributes$width(sizePx),
					elm$svg$Svg$Attributes$height(sizePx),
					elm$svg$Svg$Attributes$viewBox('0 0 ' + (sizePx + (' ' + sizePx))),
					elm$svg$Svg$Attributes$shapeRendering('crispEdges'),
					elm$svg$Svg$Attributes$stroke('#000'),
					elm$svg$Svg$Attributes$strokeWidth(
					elm$core$String$fromInt(pablohirafuji$elm_qrcode$QRCode$Render$Svg$moduleSize) + 'px')
				]),
			function (d) {
				return _List_fromArray(
					[
						A2(
						elm$svg$Svg$path,
						_List_fromArray(
							[
								d,
								elm$svg$Svg$Attributes$transform(
								'translate(' + (elm$core$String$fromInt(quietZonePx) + (', ' + (elm$core$String$fromFloat(quietZonePx + (pablohirafuji$elm_qrcode$QRCode$Render$Svg$moduleSize / 2)) + ')')))),
								elm$svg$Svg$Attributes$style('stroke-width: 5px')
							]),
						_List_Nil)
					]);
			}(
				elm$svg$Svg$Attributes$d(
					elm$core$String$concat(
						elm$core$List$concat(
							A2(
								elm$core$List$indexedMap,
								pablohirafuji$elm_qrcode$QRCode$Render$Svg$viewRow(quietZoneSize),
								A2(
									elm$core$List$map,
									A2(
										elm$core$Basics$composeR,
										A2(
											elm$core$List$foldl,
											pablohirafuji$elm_qrcode$QRCode$Render$Svg$toRowLines,
											_Utils_Tuple2(
												{space: 0, width: 0},
												_List_Nil)),
										A2(elm$core$Basics$composeR, pablohirafuji$elm_qrcode$QRCode$Render$Svg$appendLastRect, elm$core$List$reverse)),
									matrix)))))));
	});
var pablohirafuji$elm_qrcode$QRCode$Render$Svg$view = function (matrix) {
	return A2(pablohirafuji$elm_qrcode$QRCode$Render$Svg$viewBase, 4, matrix);
};
var pablohirafuji$elm_qrcode$QRCode$toSvg = function (_n0) {
	var qrCode = _n0.a;
	return pablohirafuji$elm_qrcode$QRCode$Render$Svg$view(qrCode);
};
var author$project$Main$qrCodeView = function (message) {
	return A2(
		elm$core$Result$withDefault,
		elm$html$Html$text('Error while encoding to QRCode.'),
		A2(
			elm$core$Result$map,
			pablohirafuji$elm_qrcode$QRCode$toSvg,
			pablohirafuji$elm_qrcode$QRCode$encode(message)));
};
var elm$html$Html$div = _VirtualDom_node('div');
var elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			elm$json$Json$Encode$string(string));
	});
var elm$html$Html$Attributes$class = elm$html$Html$Attributes$stringProperty('className');
var elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var elm$html$Html$Attributes$style = elm$virtual_dom$VirtualDom$style;
var author$project$Main$warning = function (txt) {
	if (txt === '') {
		return elm$html$Html$text('');
	} else {
		return A2(
			elm$html$Html$div,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class('alert alert-warning'),
					A2(elm$html$Html$Attributes$style, 'width', '30%'),
					A2(elm$html$Html$Attributes$style, 'margin', 'auto'),
					A2(elm$html$Html$Attributes$style, 'margin-top', '20px')
				]),
			_List_fromArray(
				[
					elm$html$Html$text(txt)
				]));
	}
};
var elm$html$Html$br = _VirtualDom_node('br');
var elm$html$Html$button = _VirtualDom_node('button');
var elm$html$Html$h1 = _VirtualDom_node('h1');
var elm$html$Html$h2 = _VirtualDom_node('h2');
var elm$html$Html$h3 = _VirtualDom_node('h3');
var elm$html$Html$img = _VirtualDom_node('img');
var elm$html$Html$input = _VirtualDom_node('input');
var elm$html$Html$label = _VirtualDom_node('label');
var elm$html$Html$li = _VirtualDom_node('li');
var elm$html$Html$nav = _VirtualDom_node('nav');
var elm$html$Html$ul = _VirtualDom_node('ul');
var elm$json$Json$Encode$bool = _Json_wrap;
var elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			elm$json$Json$Encode$bool(bool));
	});
var elm$html$Html$Attributes$disabled = elm$html$Html$Attributes$boolProperty('disabled');
var elm$html$Html$Attributes$for = elm$html$Html$Attributes$stringProperty('htmlFor');
var elm$html$Html$Attributes$id = elm$html$Html$Attributes$stringProperty('id');
var elm$html$Html$Attributes$src = function (url) {
	return A2(
		elm$html$Html$Attributes$stringProperty,
		'src',
		_VirtualDom_noJavaScriptOrHtmlUri(url));
};
var elm$html$Html$Attributes$type_ = elm$html$Html$Attributes$stringProperty('type');
var elm$html$Html$Attributes$value = elm$html$Html$Attributes$stringProperty('value');
var elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 'Normal', a: a};
};
var elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			elm$virtual_dom$VirtualDom$on,
			event,
			elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var elm$html$Html$Events$onClick = function (msg) {
	return A2(
		elm$html$Html$Events$on,
		'click',
		elm$json$Json$Decode$succeed(msg));
};
var elm$html$Html$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 'MayStopPropagation', a: a};
};
var elm$html$Html$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			elm$virtual_dom$VirtualDom$on,
			event,
			elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
var elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3(elm$core$List$foldr, elm$json$Json$Decode$field, decoder, fields);
	});
var elm$html$Html$Events$targetValue = A2(
	elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'value']),
	elm$json$Json$Decode$string);
var elm$html$Html$Events$onInput = function (tagger) {
	return A2(
		elm$html$Html$Events$stopPropagationOn,
		'input',
		A2(
			elm$json$Json$Decode$map,
			elm$html$Html$Events$alwaysStop,
			A2(elm$json$Json$Decode$map, tagger, elm$html$Html$Events$targetValue)));
};
var author$project$Main$view = function (model) {
	return A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				A2(elm$html$Html$Attributes$style, 'text-align', 'center')
			]),
		_List_fromArray(
			[
				A2(
				elm$html$Html$nav,
				_List_fromArray(
					[
						elm$html$Html$Attributes$class('navbar navbar-expand-sm bg-success navbar-dark')
					]),
				_List_fromArray(
					[
						A2(
						elm$html$Html$button,
						_List_fromArray(
							[
								elm$html$Html$Attributes$class('btn btn-link nav-link navbar-brand'),
								elm$html$Html$Events$onClick(
								author$project$Main$SwitchMode(author$project$Main$None))
							]),
						_List_fromArray(
							[
								elm$html$Html$text('OTP App')
							])),
						A2(
						elm$html$Html$ul,
						_List_fromArray(
							[
								elm$html$Html$Attributes$class('navbar-nav ml-auto')
							]),
						_List_fromArray(
							[
								A2(
								elm$html$Html$li,
								_List_fromArray(
									[
										elm$html$Html$Attributes$class('nav-item')
									]),
								_List_fromArray(
									[
										A2(
										elm$html$Html$button,
										_List_fromArray(
											[
												elm$html$Html$Attributes$class('btn btn-link nav-link'),
												elm$html$Html$Events$onClick(
												author$project$Main$SwitchMode(author$project$Main$SignUp))
											]),
										_List_fromArray(
											[
												elm$html$Html$text('Sign Up')
											]))
									])),
								A2(
								elm$html$Html$li,
								_List_fromArray(
									[
										elm$html$Html$Attributes$class('nav-item')
									]),
								_List_fromArray(
									[
										A2(
										elm$html$Html$button,
										_List_fromArray(
											[
												elm$html$Html$Attributes$class('btn btn-link nav-link'),
												elm$html$Html$Events$onClick(
												author$project$Main$SwitchMode(author$project$Main$SignIn))
											]),
										_List_fromArray(
											[
												elm$html$Html$text('Sign In')
											]))
									]))
							]))
					])),
				function () {
				var _n0 = model.state;
				switch (_n0.$) {
					case 'NoLogin':
						var _n1 = model.mode;
						switch (_n1.$) {
							case 'SignUp':
								return A2(
									elm$html$Html$div,
									_List_fromArray(
										[
											A2(elm$html$Html$Attributes$style, 'margin-top', '30px')
										]),
									_List_fromArray(
										[
											A2(
											elm$html$Html$h1,
											_List_Nil,
											_List_fromArray(
												[
													elm$html$Html$text('Register your account')
												])),
											A2(
											elm$html$Html$div,
											_List_fromArray(
												[
													elm$html$Html$Attributes$class('text-muted')
												]),
											_List_fromArray(
												[
													elm$html$Html$text('Password must be at least 6 characters long')
												])),
											A2(
											elm$html$Html$div,
											_List_Nil,
											_List_fromArray(
												[
													A2(
													elm$html$Html$label,
													_List_fromArray(
														[
															elm$html$Html$Attributes$for('name')
														]),
													_List_fromArray(
														[
															elm$html$Html$text('Username: ')
														])),
													A2(
													elm$html$Html$input,
													_List_fromArray(
														[
															elm$html$Html$Attributes$class('form-control'),
															A2(elm$html$Html$Attributes$style, 'width', '50%'),
															A2(elm$html$Html$Attributes$style, 'margin', 'auto'),
															elm$html$Html$Attributes$id('name'),
															elm$html$Html$Attributes$value(model.name),
															elm$html$Html$Events$onInput(author$project$Main$Name),
															elm$html$Html$Attributes$type_('text')
														]),
													_List_Nil)
												])),
											A2(
											elm$html$Html$div,
											_List_Nil,
											_List_fromArray(
												[
													A2(
													elm$html$Html$label,
													_List_fromArray(
														[
															elm$html$Html$Attributes$for('pass')
														]),
													_List_fromArray(
														[
															elm$html$Html$text('Password: ')
														])),
													A2(
													elm$html$Html$input,
													_List_fromArray(
														[
															elm$html$Html$Attributes$class('form-control'),
															A2(elm$html$Html$Attributes$style, 'width', '50%'),
															A2(elm$html$Html$Attributes$style, 'margin', 'auto'),
															elm$html$Html$Attributes$id('pass'),
															elm$html$Html$Attributes$value(model.password),
															elm$html$Html$Events$onInput(author$project$Main$Password),
															elm$html$Html$Attributes$type_('password')
														]),
													_List_Nil)
												])),
											A2(
											elm$html$Html$div,
											_List_Nil,
											_List_fromArray(
												[
													A2(
													elm$html$Html$button,
													_List_fromArray(
														[
															elm$html$Html$Attributes$class('btn btn-success'),
															A2(elm$html$Html$Attributes$style, 'margin-top', '30px'),
															function () {
															var _n2 = model.secret;
															if (_n2.$ === 'Received') {
																return elm$html$Html$Attributes$disabled(true);
															} else {
																return elm$html$Html$Attributes$class('');
															}
														}(),
															elm$html$Html$Events$onClick(author$project$Main$Register)
														]),
													_List_fromArray(
														[
															elm$html$Html$text('Register')
														]))
												])),
											author$project$Main$warning(model.warning),
											function () {
											var _n3 = model.secret;
											if (_n3.$ === 'Received') {
												var secret = _n3.a;
												return A2(
													elm$html$Html$div,
													_List_Nil,
													_List_fromArray(
														[
															A2(
															elm$html$Html$h3,
															_List_fromArray(
																[
																	A2(elm$html$Html$Attributes$style, 'margin-top', '20px')
																]),
															_List_fromArray(
																[
																	elm$html$Html$text('Scan your QR Code through the Google Authenticator app'),
																	A2(elm$html$Html$br, _List_Nil, _List_Nil),
																	elm$html$Html$text('Store the secret key carefully! Do not leave this page without scanning the code')
																])),
															author$project$Main$qrCodeView(secret),
															A2(
															elm$html$Html$div,
															_List_fromArray(
																[
																	elm$html$Html$Attributes$class('text-muted')
																]),
															_List_fromArray(
																[
																	elm$html$Html$text('Press confirm after scanning the code')
																])),
															A2(
															elm$html$Html$div,
															_List_fromArray(
																[
																	A2(elm$html$Html$Attributes$style, 'margin-top', '20px')
																]),
															_List_fromArray(
																[
																	A2(
																	elm$html$Html$button,
																	_List_fromArray(
																		[
																			elm$html$Html$Attributes$class('btn btn-success'),
																			elm$html$Html$Events$onClick(
																			author$project$Main$SwitchMode(author$project$Main$SignIn))
																		]),
																	_List_fromArray(
																		[
																			elm$html$Html$text('Confirm')
																		]))
																]))
														]));
											} else {
												return elm$html$Html$text('');
											}
										}()
										]));
							case 'SignIn':
								return A2(
									elm$html$Html$div,
									_List_fromArray(
										[
											A2(elm$html$Html$Attributes$style, 'margin-top', '30px')
										]),
									_List_fromArray(
										[
											A2(
											elm$html$Html$h1,
											_List_Nil,
											_List_fromArray(
												[
													elm$html$Html$text('Log In')
												])),
											A2(
											elm$html$Html$div,
											_List_Nil,
											_List_fromArray(
												[
													A2(
													elm$html$Html$label,
													_List_fromArray(
														[
															elm$html$Html$Attributes$for('name')
														]),
													_List_fromArray(
														[
															elm$html$Html$text('Username: ')
														])),
													A2(
													elm$html$Html$input,
													_List_fromArray(
														[
															elm$html$Html$Attributes$class('form-control'),
															A2(elm$html$Html$Attributes$style, 'width', '50%'),
															A2(elm$html$Html$Attributes$style, 'margin', 'auto'),
															elm$html$Html$Attributes$id('name'),
															elm$html$Html$Attributes$value(model.name),
															elm$html$Html$Events$onInput(author$project$Main$Name),
															elm$html$Html$Attributes$type_('text')
														]),
													_List_Nil)
												])),
											A2(
											elm$html$Html$div,
											_List_Nil,
											_List_fromArray(
												[
													A2(
													elm$html$Html$label,
													_List_fromArray(
														[
															elm$html$Html$Attributes$for('pass')
														]),
													_List_fromArray(
														[
															elm$html$Html$text('Password: ')
														])),
													A2(
													elm$html$Html$input,
													_List_fromArray(
														[
															elm$html$Html$Attributes$class('form-control'),
															A2(elm$html$Html$Attributes$style, 'width', '50%'),
															A2(elm$html$Html$Attributes$style, 'margin', 'auto'),
															elm$html$Html$Attributes$id('pass'),
															elm$html$Html$Attributes$value(model.password),
															elm$html$Html$Events$onInput(author$project$Main$Password),
															elm$html$Html$Attributes$type_('password')
														]),
													_List_Nil)
												])),
											A2(
											elm$html$Html$div,
											_List_Nil,
											_List_fromArray(
												[
													A2(
													elm$html$Html$button,
													_List_fromArray(
														[
															elm$html$Html$Attributes$class('btn btn-success'),
															A2(elm$html$Html$Attributes$style, 'margin-top', '30px'),
															elm$html$Html$Events$onClick(author$project$Main$LogIn)
														]),
													_List_fromArray(
														[
															elm$html$Html$text('Log In')
														]))
												])),
											author$project$Main$warning(model.warning)
										]));
							default:
								return A2(
									elm$html$Html$div,
									_List_fromArray(
										[
											A2(elm$html$Html$Attributes$style, 'margin-top', '30px')
										]),
									_List_fromArray(
										[
											A2(
											elm$html$Html$h1,
											_List_Nil,
											_List_fromArray(
												[
													elm$html$Html$text('Welcome to OTP testing app')
												])),
											A2(
											elm$html$Html$div,
											_List_fromArray(
												[
													elm$html$Html$Attributes$class('text-muted')
												]),
											_List_fromArray(
												[
													elm$html$Html$text('This application was created to test TOTP implementation')
												])),
											A2(
											elm$html$Html$h2,
											_List_Nil,
											_List_fromArray(
												[
													elm$html$Html$text('Usage')
												])),
											A2(
											elm$html$Html$div,
											_List_fromArray(
												[
													elm$html$Html$Attributes$class('text-muted')
												]),
											_List_fromArray(
												[
													elm$html$Html$text('Make sure you have MongoDB database installed and run the server through node.js'),
													A2(elm$html$Html$br, _List_Nil, _List_Nil),
													elm$html$Html$text('Make sure you have all the npm packages installed (npm install)'),
													A2(elm$html$Html$br, _List_Nil, _List_Nil),
													elm$html$Html$text('Use the command \"node server.js\" to start the server on port 3000'),
													A2(elm$html$Html$br, _List_Nil, _List_Nil),
													elm$html$Html$text('Sign up and scan the secret QRcode to Google Authenticator application'),
													A2(elm$html$Html$br, _List_Nil, _List_Nil),
													elm$html$Html$text('Next, try signing in using all your creditentials!')
												]))
										]));
						}
					case 'AwaitOTP':
						var string = _n0.a;
						return A2(
							elm$html$Html$div,
							_List_fromArray(
								[
									A2(elm$html$Html$Attributes$style, 'margin-top', '20px')
								]),
							_List_fromArray(
								[
									A2(
									elm$html$Html$h3,
									_List_Nil,
									_List_fromArray(
										[
											elm$html$Html$text('Almost done, ' + (string + '!'))
										])),
									A2(
									elm$html$Html$div,
									_List_fromArray(
										[
											elm$html$Html$Attributes$class('text-muted')
										]),
									_List_fromArray(
										[
											elm$html$Html$text('Enter your OTP to complete your login')
										])),
									A2(
									elm$html$Html$div,
									_List_Nil,
									_List_fromArray(
										[
											A2(
											elm$html$Html$label,
											_List_fromArray(
												[
													elm$html$Html$Attributes$for('code')
												]),
											_List_fromArray(
												[
													elm$html$Html$text('OTP: ')
												])),
											A2(
											elm$html$Html$input,
											_List_fromArray(
												[
													elm$html$Html$Attributes$class('form-control'),
													A2(elm$html$Html$Attributes$style, 'width', '50%'),
													A2(elm$html$Html$Attributes$style, 'margin', 'auto'),
													elm$html$Html$Attributes$id('code'),
													elm$html$Html$Attributes$value(model.code),
													elm$html$Html$Events$onInput(author$project$Main$Code),
													elm$html$Html$Attributes$type_('text')
												]),
											_List_Nil)
										])),
									A2(
									elm$html$Html$button,
									_List_fromArray(
										[
											elm$html$Html$Attributes$class('btn btn-success'),
											A2(elm$html$Html$Attributes$style, 'margin-top', '20px'),
											elm$html$Html$Events$onClick(author$project$Main$OTP)
										]),
									_List_fromArray(
										[
											elm$html$Html$text('Submit')
										])),
									author$project$Main$warning(model.warning)
								]));
					default:
						var string = _n0.a;
						return A2(
							elm$html$Html$div,
							_List_fromArray(
								[
									A2(elm$html$Html$Attributes$style, 'margin-top', '30px')
								]),
							_List_fromArray(
								[
									A2(
									elm$html$Html$h1,
									_List_Nil,
									_List_fromArray(
										[
											elm$html$Html$text('Welcome, ' + string)
										])),
									A2(
									elm$html$Html$div,
									_List_fromArray(
										[
											elm$html$Html$Attributes$class('text-muted')
										]),
									_List_fromArray(
										[
											elm$html$Html$text('Congratulations, your authentication was successfull')
										])),
									A2(
									elm$html$Html$img,
									_List_fromArray(
										[
											elm$html$Html$Attributes$src('assets/congrats.jpg')
										]),
									_List_Nil)
								]));
				}
			}()
			]));
};
var elm$browser$Browser$External = function (a) {
	return {$: 'External', a: a};
};
var elm$browser$Browser$Internal = function (a) {
	return {$: 'Internal', a: a};
};
var elm$browser$Browser$Dom$NotFound = function (a) {
	return {$: 'NotFound', a: a};
};
var elm$core$Basics$never = function (_n0) {
	never:
	while (true) {
		var nvr = _n0.a;
		var $temp$_n0 = nvr;
		_n0 = $temp$_n0;
		continue never;
	}
};
var elm$core$Task$Perform = function (a) {
	return {$: 'Perform', a: a};
};
var elm$core$Task$init = elm$core$Task$succeed(_Utils_Tuple0);
var elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			elm$core$Task$andThen,
			function (a) {
				return elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var elm$core$Task$spawnCmd = F2(
	function (router, _n0) {
		var task = _n0.a;
		return _Scheduler_spawn(
			A2(
				elm$core$Task$andThen,
				elm$core$Platform$sendToApp(router),
				task));
	});
var elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			elm$core$Task$map,
			function (_n0) {
				return _Utils_Tuple0;
			},
			elm$core$Task$sequence(
				A2(
					elm$core$List$map,
					elm$core$Task$spawnCmd(router),
					commands)));
	});
var elm$core$Task$onSelfMsg = F3(
	function (_n0, _n1, _n2) {
		return elm$core$Task$succeed(_Utils_Tuple0);
	});
var elm$core$Task$cmdMap = F2(
	function (tagger, _n0) {
		var task = _n0.a;
		return elm$core$Task$Perform(
			A2(elm$core$Task$map, tagger, task));
	});
_Platform_effectManagers['Task'] = _Platform_createManager(elm$core$Task$init, elm$core$Task$onEffects, elm$core$Task$onSelfMsg, elm$core$Task$cmdMap);
var elm$core$Task$command = _Platform_leaf('Task');
var elm$core$Task$perform = F2(
	function (toMessage, task) {
		return elm$core$Task$command(
			elm$core$Task$Perform(
				A2(elm$core$Task$map, toMessage, task)));
	});
var elm$core$String$slice = _String_slice;
var elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			elm$core$String$slice,
			n,
			elm$core$String$length(string),
			string);
	});
var elm$core$String$startsWith = _String_startsWith;
var elm$url$Url$Http = {$: 'Http'};
var elm$url$Url$Https = {$: 'Https'};
var elm$core$String$indexes = _String_indexes;
var elm$core$String$isEmpty = function (string) {
	return string === '';
};
var elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3(elm$core$String$slice, 0, n, string);
	});
var elm$core$String$contains = _String_contains;
var elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {fragment: fragment, host: host, path: path, port_: port_, protocol: protocol, query: query};
	});
var elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if (elm$core$String$isEmpty(str) || A2(elm$core$String$contains, '@', str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, ':', str);
			if (!_n0.b) {
				return elm$core$Maybe$Just(
					A6(elm$url$Url$Url, protocol, str, elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_n0.b.b) {
					var i = _n0.a;
					var _n1 = elm$core$String$toInt(
						A2(elm$core$String$dropLeft, i + 1, str));
					if (_n1.$ === 'Nothing') {
						return elm$core$Maybe$Nothing;
					} else {
						var port_ = _n1;
						return elm$core$Maybe$Just(
							A6(
								elm$url$Url$Url,
								protocol,
								A2(elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return elm$core$Maybe$Nothing;
				}
			}
		}
	});
var elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if (elm$core$String$isEmpty(str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, '/', str);
			if (!_n0.b) {
				return A5(elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _n0.a;
				return A5(
					elm$url$Url$chompBeforePath,
					protocol,
					A2(elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2(elm$core$String$left, i, str));
			}
		}
	});
var elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if (elm$core$String$isEmpty(str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, '?', str);
			if (!_n0.b) {
				return A4(elm$url$Url$chompBeforeQuery, protocol, elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _n0.a;
				return A4(
					elm$url$Url$chompBeforeQuery,
					protocol,
					elm$core$Maybe$Just(
						A2(elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2(elm$core$String$left, i, str));
			}
		}
	});
var elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if (elm$core$String$isEmpty(str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, '#', str);
			if (!_n0.b) {
				return A3(elm$url$Url$chompBeforeFragment, protocol, elm$core$Maybe$Nothing, str);
			} else {
				var i = _n0.a;
				return A3(
					elm$url$Url$chompBeforeFragment,
					protocol,
					elm$core$Maybe$Just(
						A2(elm$core$String$dropLeft, i + 1, str)),
					A2(elm$core$String$left, i, str));
			}
		}
	});
var elm$url$Url$fromString = function (str) {
	return A2(elm$core$String$startsWith, 'http://', str) ? A2(
		elm$url$Url$chompAfterProtocol,
		elm$url$Url$Http,
		A2(elm$core$String$dropLeft, 7, str)) : (A2(elm$core$String$startsWith, 'https://', str) ? A2(
		elm$url$Url$chompAfterProtocol,
		elm$url$Url$Https,
		A2(elm$core$String$dropLeft, 8, str)) : elm$core$Maybe$Nothing);
};
var elm$browser$Browser$element = _Browser_element;
var author$project$Main$main = elm$browser$Browser$element(
	{init: author$project$Main$init, subscriptions: author$project$Main$subscriptions, update: author$project$Main$update, view: author$project$Main$view});
_Platform_export({'Main':{'init':author$project$Main$main(
	elm$json$Json$Decode$succeed(_Utils_Tuple0))(0)}});}(this));