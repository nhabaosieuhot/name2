return (function(...)
	local r = {
			"\053\121\087\097\070\043\087\071\110\066\120\082\110\043\112\043",
			"\110\066\120\103\053\077\050\089",
			"\086\079\106\122\086\084\106\105\116\079\106\067\086\084\104\067\107\052\057\061",
			"\107\077\050\048\116\077\087\056",
			"\101\077\050\043\086\077\087\122\086\077\076\061",
			"\070\077\104\122\116\079\068\061",
			"\057\052\068\103\076\051\074\061",
			"\110\055\071\077\110\066\106\080\106\077\055\102\107\084\107\052\065\074\061\061",
			"\121\055\112\089\116\048\061\061",
			"\086\079\106\122\070\077\106\090\070\066\072\050\107\043\104\075\107\077\108\061",
			"\086\043\071\067\086\084\086\047\110\089\087\122\116\079\097\047\070\084\076\061",
			"\110\121\107\082\110\043\076\061",
			"\110\084\112\047\070\089\120\071\110\071\112\122\070\055\112\055\110\079\106\103\121\079\120\097\107\084\088\061",
			"\101\089\106\089\086\048\061\061",
			"\119\077\106\105\053\104\085\097\110\089\076\061",
			"\076\108\111\061",
			"\086\043\080\082\116\121\076\061",
			"\086\079\106\122\070\043\104\090\086\076\061\061",
			"\106\084\112\082\070\052\057\061",
			"\108\084\104\103\107\074\061\061",
			"\086\079\106\122\116\079\080\097\110\066\087\067\116\077\055\071",
			"\121\055\112\047\070\043\120\071\109\074\061\061",
			"\110\079\106\122\070\077\106\090\070\066\072\050\107\043\104\075\107\077\108\061",
			"\057\052\068\080\087\105\068\061",
			"\119\077\112\111\086\077\080\105",
			"\101\084\106\097\086\074\061\061",
			"\108\043\106\048\070\084\071\051\116\121\120\071\086\104\087\122\070\066\072\097\086\079\108\061",
			"\076\121\087\105\086\121\120\105",
			"\110\084\087\097\070\084\048\061",
			"\121\055\112\075\086\077\069\061",
			"\120\079\104\090\086\076\061\061",
		};
	for A, J in ipairs({ { 1, 31 }, { 1, 9 }, { 10, 31 } }) do
		while J[1] < J[2] do
			r[J[1]], r[J[2]], J[1], J[2] = r[J[2]], r[J[1]], J[1] + 1, J[2] - 1;
		end;
	end;
	local function A(A)
		return r[A + 64195];
	end;
	do
		local A = string.char;
		local J = {
				f = 3,
				Y = 39,
				L = 16,
				I = 11,
				e = 18,
				["\053"] = 26,
				k = 29,
				["\057"] = 12,
				v = 31,
				l = 20,
				N = 59,
				["\055"] = 53,
				y = 23,
				["\043"] = 38,
				Z = 45,
				K = 44,
				c = 34,
				a = 33,
				g = 50,
				E = 56,
				T = 6,
				B = 55,
				F = 27,
				["\051"] = 35,
				P = 49,
				R = 47,
				i = 51,
				x = 17,
				["\048"] = 48,
				t = 24,
				G = 37,
				V = 25,
				J = 0,
				n = 28,
				Q = 42,
				q = 58,
				W = 13,
				M = 22,
				C = 46,
				p = 61,
				j = 21,
				z = 52,
				S = 63,
				X = 4,
				w = 19,
				m = 30,
				d = 60,
				r = 62,
				U = 1,
				u = 10,
				["\050"] = 57,
				H = 9,
				["\047"] = 41,
				D = 32,
				A = 14,
				h = 5,
				O = 54,
				s = 2,
				["\056"] = 43,
				["\049"] = 8,
				b = 40,
				o = 36,
				["\052"] = 7,
				["\054"] = 15,
			};
		local z = string.len;
		local S = type;
		local b = math.floor;
		local Z = table.concat;
		local U = string.sub;
		local q = r;
		local f = table.insert;
		for r = 1, #q, 1 do
			local h = q[r];
			if S(h) == "\115\116\114\105\110\103" then
				local S = z(h);
				local T = {};
				local d = 1;
				local K = 0;
				local j = 0;
				while d <= S do
					local r = U(h, d, d);
					local z = J[r];
					if z then
						K = K + z * 64 ^ (3 - j);
						j = j + 1;
						if j == 4 then
							j = 0;
							local r = b(K / 65536);
							local J = b((K % 65536) / 256);
							local z = K % 256;
							f(T, A(r, J, z));
							K = 0;
						end;
					elseif r == "\061" then
						f(T, A(b(K / 65536)));
						if d >= S or U(h, d + 1, d + 1) ~= "\061" then
							f(T, A(b((K % 65536) / 256)));
						end;
						break;
					end;
					d = d + 1;
				end;
				q[r] = Z(T);
			end;
		end;
	end;
	return (function(r, z, S, b, Z, U, q, f, d, o, K, G, j, J, h, T)
		d, K, o, j, J, T, f, G, h = 0, function(r)
				for A = 1, #r, 1 do
					h[r[A]] = h[r[A]] + 1;
				end;
				if S then
					local J = S(true);
					local z = Z(J);
					z[A(-64164)], z[A(-64177)], z[A(-64187)] = r, j, function()
							return 2077632;
						end;
					return J;
				else
					return b({}, { [A(-64177)] = j, [A(-64164)] = r, [A(-64187)] = function()
							return 2077632;
						end });
				end;
			end, function(r, A)
				local z = K(A);
				local S = function(...)
						return J(r, { ... }, A, z);
					end;
				return S;
			end, function(r)
				local A, J = 1, r[1];
				while J do
					h[J], A = h[J] - 1, 1 + A;
					if h[J] == 0 then
						h[J], f[J] = nil, nil;
					end;
					J = r[A];
				end;
			end, function(J, S, b, Z)
				local M, f, F, m, j, e, g, k, i, s, t, R, O, G, Y, o, q, d, T, K, h, E;
				while J do
					if J < 9119383 then
						if J < 4186670 then
							if J < 2663123 then
								if J < 1559915 then
									if J < 363654 then
										m = nil;
										R = nil;
										k = nil;
										J = 2693714;
										t = nil;
									else
										E = A(-64165);
										g = r[E];
										m = J;
										E = g(F);
										g = A(-64166);
										R = E == g;
										k = R;
										J = R and 7149786 or 3327589;
									end;
								else
									R = A(-64185);
									m = r[R];
									R = m(K, F);
									J = 9008403;
									k = not R;
									t = k;
								end;
							else
								if J < 3308883 then
									J = 10489152;
								else
									E = A(-64165);
									g = r[E];
									E = g(F);
									g = A(-64171);
									R = E == g;
									k = R;
									J = 7149786;
								end;
							end;
						else
							if J < 5970940 then
								if J < 5216967 then
									g = A(-64168);
									e = i;
									R = A(-64184);
									m = r[R];
									R = A(-64180);
									k = m[R];
									R = r[g];
									g = R(F);
									R = A(-64191);
									m = k(g, R);
									J = m and 1307687 or 14592958;
									t = m;
								else
									R = A(-64185);
									m = r[R];
									R = m(j, F);
									k = not R;
									J = k and 1793078 or 9008403;
									t = k;
								end;
							else
								if J < 7836409 then
									J = m;
									J = 14592958;
									t = k;
								else
									J = t and 16301554 or 2693714;
								end;
							end;
						end;
					else
						if J < 12339027 then
							if J < 11503933 then
								if J < 10649058 then
									if J < 9254253 then
										O = A(-64169);
										g = A(-64194);
										J = r[g];
										Y = 10;
										E = A(-64179);
										g = J(R, E, O, Y);
										Y = 10;
										g = A(-64194);
										O = A(-64169);
										E = 692;
										J = r[g];
										g = J(R, E, O, Y);
										E = 696;
										O = A(-64169);
										Y = 10;
										g = A(-64194);
										J = r[g];
										g = J(R, E, O, Y);
										J = 216070;
									else
										e = nil;
										J = 13943722;
										F = nil;
									end;
								else
									G = nil;
									o = nil;
									J = r[A(-64178)];
									T = nil;
									q = {};
									K = nil;
									h = nil;
									d = nil;
									j = nil;
								end;
							else
								if J < 12164735 then
									J = 12191604;
									g = m;
								else
									J = g and 9235738 or 216070;
								end;
							end;
						else
							if J < 14590927 then
								if J < 14028447 then
									i, F = M(s, i);
									J = i and 4386438 or 10668358;
								else
									d = A(-64170);
									h = A(-64186);
									q = A(-64175);
									J = r[q];
									T = r[h];
									h = A(-64190);
									f = S;
									o = A(-64167);
									q = J(T, h);
									T = q;
									K = A(-64181);
									q = A(-64175);
									J = r[q];
									h = A(-64189);
									q = J(T, h);
									h = q;
									G = A(-64172);
									q = A(-64175);
									J = r[q];
									q = J(h, d);
									d = q;
									j = A(-64192);
									q = A(-64175);
									J = r[q];
									q = J(d, K);
									K = q;
									q = A(-64175);
									J = r[q];
									q = J(T, j);
									j = q;
									q = A(-64175);
									J = r[q];
									q = J(j, G);
									G = q;
									q = A(-64175);
									J = r[q];
									q = J(j, o);
									o = q;
									q = A(-64183);
									J = r[q];
									M = { J(T) };
									q = M[1];
									i = M[3];
									s = M[2];
									M = q;
									J = 13943722;
								end;
							else
								if J < 15020173 then
									J = t and 5232800 or 10489152;
								else
									R = A(-64176);
									t = A(-64188);
									J = r[t];
									m = r[R];
									R = A(-64193);
									g = A(-64174);
									E = { J(m, F, R, g) };
									m = A(-64188);
									k = E[2];
									J = r[m];
									t = E[1];
									E = A(-64173);
									g = r[E];
									E = { J(g, k) };
									m = E[1];
									g = t;
									R = E[2];
									J = t and 12125554 or 12191604;
								end;
							end;
						end;
					end;
				end;
				J = #Z;
				return z(q);
			end, function()
				d = d + 1;
				h[d] = 1;
				return d;
			end, {}, function(r)
				h[r] = h[r] - 1;
				if 0 == h[r] then
					h[r], f[r] = nil, nil;
				end;
			end, {};
		return (o(14522812, {}))(z(q));
	end)(getfenv and getfenv() or _ENV, unpack or table[A(-64182)], newproxy, setmetatable, getmetatable, select, { ... });
end)(...);