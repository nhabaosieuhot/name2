return (function(...)
	local varargs = {...}
	local M = {
			"\103\081\104\065\068\055\069\061",
			"\117\089\085\082\114\089\075\110\114\089\119\061",
			"\108\090\043\065\108\082\119\061",
			"\102\082\103\067\068\081\121\107\051\090\101\121\114\056\075\110\068\105\078\054\114\057\102\061",
			"\114\082\072\114\114\122\080\122\068\121\054\111\089\082\085\118\068\116\061\061",
			"\087\119\061\061",
			"\090\071\104\100\114\089\049\061",
			"\068\089\056\110\051\057\116\061",
			"\043\089\085\067\051\089\075\099",
			"\069\055\116\088\119\107\112\061",
			"\113\105\054\082\122\103\103\076\102\079\116\048\075\121\075\121\115\090\120\061",
			"\122\089\104\111\114\089\072\048",
			"\102\081\056\088\043\112\061\061",
			"\101\057\103\110",
			"\087\090\103\085\102\110\106\071\119\122\103\079\102\090\073\067\119\048\111\061",
			"\117\081\103\054\114\112\061\061",
			"\069\055\116\072\075\048\116\061",
			"\122\089\103\048\113\056\080\054\108\074\119\061",
			"\068\110\111\072\114\057\108\105\103\079\121\100\113\057\104\101\103\067\061\061",
			"\102\081\056\088\043\056\075\050\087\082\102\061",
			"\114\057\103\110\051\057\072\054\108\105\075\076\051\089\071\121",
			"\119\102\111\061",
			"\114\057\103\110\068\089\103\098\068\105\078\085\043\082\056\100\043\089\102\061",
			"\117\074\103\074\114\067\061\061",
			"\113\090\075\054\068\082\075\121\108\105\101\065\108\082\104\082",
			"\089\112\061\061",
			"\102\055\078\050\068\089\121\110\113\090\114\121",
			"\108\057\103\110\068\089\103\098\068\105\078\085\043\082\056\100\043\089\102\061",
			"\087\116\061\061",
			"\114\057\103\110\114\081\103\048\051\057\103\076\114\081\056\076\043\055\069\061",
			"\108\081\104\050\068\074\101\121\108\121\104\110\068\071\104\071\108\057\103\088\090\057\101\054\043\081\120\061",
			"\114\057\103\110\068\082\056\098\114\119\061\061",
			"\089\119\061\061",
			"\090\071\104\050\068\082\101\121\087\112\061\061",
			"\089\116\061\061",
			"\108\105\101\088\113\089\085\074",
			"\090\071\104\074\051\067\061\061",
			"\114\057\103\110\108\081\056\088\043\055\075\050\087\082\102\061",
			"\114\082\121\076\114\081\114\050\108\074\075\110\051\057\054\050\068\081\119\061",
			"\101\057\056\098\114\119\061\061",
			"\119\090\075\048\114\090\101\048",
			"\114\082\072\065\051\090\119\061",
			"\108\057\103\110\108\081\056\088\043\055\075\050\087\082\102\061",
			"\087\112\061\061",
		};
	local function n(n)
		return M[n + 2910];
	end;
	for n, F in ipairs({ { 1, 44 }, { 1, 20 }, { 21, 44 } }) do
		while F[1] < F[2] do
			M[F[1]], M[F[2]], F[1], F[2] = M[F[2]], M[F[1]], F[1] + 1, F[2] - 1;
		end;
	end;
	do
		local n = M;
		local F = string.len;
		local o = table.concat;
		local m = string.char;
		local T = string.sub;
		local Y = math.floor;
		local t = type;
		local d_decode_map = {
				R = 38,
				W = 30,
				["\049"] = 56,
				e = 17,
				k = 35,
				K = 13,
				v = 58,
				f = 20,
				G = 53,
				["\057"] = 54,
				r = 25,
				Q = 6,
				["\051"] = 24,
				L = 46,
				H = 49,
				b = 45,
				E = 12,
				["\056"] = 5,
				["\055"] = 7,
				M_val = 8, -- Renamed M to M_val to avoid conflict with outer M
				c = 43,
				t_char = 32, -- Renamed t to t_char 
				C = 48,
				["\052"] = 42,
				V = 34,
				["\053"] = 59,
				Y_val = 22, -- Renamed Y to Y_val
				P = 1,
				Z = 23,
				p = 0,
				A = 47,
				["\050"] = 41,
				s = 14,
				X = 50,
				h = 61,
				I = 40,
				J = 39,
				B = 10,
				y = 37,
				["\047"] = 63,
				["\048"] = 51,
				U = 57,
				["\043"] = 29,
				m_val = 2, -- Renamed m to m_val
				l = 28,
				z = 19,
				T_val = 15, -- Renamed T to T_val
				a = 62,
				d_char = 44, -- Renamed d to d_char
				F_val = 31, -- Renamed F to F_val
				["\054"] = 33,
				i = 55,
				w = 16,
				u = 18,
				S = 11,
				x = 4,
				n_val = 52, -- Renamed n to n_val
				j = 60,
				q = 26,
				D = 27,
				g = 21,
				o_val = 36, -- Renamed o to o_val
				N = 9,
				O = 3,
			};
		d_decode_map.M = d_decode_map.M_val; d_decode_map.M_val=nil;
		d_decode_map.t = d_decode_map.t_char; d_decode_map.t_char=nil;
		d_decode_map.Y = d_decode_map.Y_val; d_decode_map.Y_val=nil;
		d_decode_map.m = d_decode_map.m_val; d_decode_map.m_val=nil;
		d_decode_map.T = d_decode_map.T_val; d_decode_map.T_val=nil;
		d_decode_map.d = d_decode_map.d_char; d_decode_map.d_char=nil;
		d_decode_map.F = d_decode_map.F_val; d_decode_map.F_val=nil;
		d_decode_map.n = d_decode_map.n_val; d_decode_map.n_val=nil;
		d_decode_map.o = d_decode_map.o_val; d_decode_map.o_val=nil;
		local R_insert = table.insert;
		for M_idx = 1, #n, 1 do
			local I_str = n[M_idx];
			if t(I_str) == "\115\116\114\105\110\103" then
				local t_len = F(I_str);
				local s_chars = {};
				local k_char_idx = 1;
				local r_accum = 0;
				local f_count = 0;
				while k_char_idx <= t_len do
					local M_char = T(I_str, k_char_idx, k_char_idx);
					local n_val_lookup = d_decode_map[M_char];
					if n_val_lookup then
						r_accum = r_accum + n_val_lookup * 64 ^ (3 - f_count);
						f_count = f_count + 1;
						if f_count == 4 then
							f_count = 0;
							local M_b1 = Y(r_accum / 65536);
							local n_b2 = Y((r_accum % 65536) / 256);
							local F_b3 = r_accum % 256;
							R_insert(s_chars, m(M_b1, n_b2, F_b3));
							r_accum = 0;
						end;
					elseif M_char == "\061" then
						R_insert(s_chars, m(Y(r_accum / 65536)));
						if k_char_idx >= t_len or T(I_str, k_char_idx + 1, k_char_idx + 1) ~= "\061" then
							R_insert(s_chars, m(Y((r_accum % 65536) / 256)));
						end;
						break;
					end;
					k_char_idx = k_char_idx + 1;
				end;
				n[M_idx] = o(s_chars);
			end;
		end;
	end;
	return (function(M_env, o_unpack, m_newproxy, T_setmeta, Y_getmeta, t_select, d_arg_varargs, F_dispatcher, s_makeslot, P_callslot, R_slotdata, p_argbuilder1, I_slotrefs, r_builddispatch, a_argbuilder2, f_const, k_counter, Q_argbuilder3, Z_decref)
		k_counter, I_slotrefs, a_argbuilder2, R_slotdata, Z_decref, F_dispatcher, f_const, Q_argbuilder3, P_callslot, r_builddispatch, s_makeslot, p_argbuilder1 = 0, {}, function(M, n_idx)
				local o_dispatch_table = r_builddispatch(n_idx);
				local m_dispatch_func = function(m_op, T_a, Y_b, t_c)
						return F_dispatcher(M, {
							m_op,
							T_a,
							Y_b,
							t_c,
						}, n_idx, o_dispatch_table);
					end;
				return m_dispatch_func;
			end, {}, function(M_slot)
				I_slotrefs[M_slot] = I_slotrefs[M_slot] - 1;
				if 0 == I_slotrefs[M_slot] then
					I_slotrefs[M_slot], R_slotdata[M_slot] = nil, nil;
				end;
			end, function(F_opcode, m_args, T_regs, Y_len)
				local d_loc, g_loc, h_loc, P_loc, w_loc, l_loc, C_loc, k_loc, L_loc, I_loc, v_loc, y_loc, r_loc, j_loc, c_loc, B_loc, q_loc, u_loc, f_loc, z_loc, A_loc, x_loc, O_loc, E_loc;
				while F_opcode do
					if F_opcode < 11476850 then
						if F_opcode < 6293455 then
							if F_opcode < 2923584 then
								if F_opcode < 1406603 then
									if F_opcode < 315307 then
										d_loc = n(-2867);
										I_loc = m_args[1];
										F_opcode = M_env[d_loc];
										f_loc = n(-2907);
										r_loc = R_slotdata[T_regs[1]];
										k_loc = r_loc[f_loc];
										r_loc = n(-2887);
										d_loc = F_opcode(I_loc, k_loc, r_loc);
										k_loc = d_loc;
										d_loc = n(-2903);
										F_opcode = M_env[d_loc];
										d_loc = { F_opcode(k_loc) };
										F_opcode = M_env[n(-2871)];
										d_loc = { o_unpack(d_loc) };
									else
										g_loc = nil;
										r_loc = Z_decref(r_loc);
										h_loc = nil;
										v_loc = nil;
										d_loc = {};
										F_opcode = M_env[n(-2885)];
										P_loc = nil;
										k_loc = Z_decref(k_loc);
										A_loc = nil;
										u_loc = nil;
										f_loc = nil;
									end;
								else
									E_loc = n(-2909);
									c_loc = M_env[E_loc];
									E_loc = c_loc(g_loc, w_loc);
									O_loc = not E_loc;
									F_opcode = O_loc and 12853129 or 3803764;
									C_loc = O_loc;
								end;
							else
								if F_opcode < 5125974 then
									F_opcode = C_loc and 16312818 or 11224919;
								else
									A_loc = n(-2868);
									I_loc = m_args;
									k_loc = n(-2873);
									d_loc = n(-2907);
									r_loc = n(-2870);
									f_loc = n(-2880);
									F_opcode = { [d_loc] = k_loc, [r_loc] = f_loc };
									k_loc = s_makeslot();
									r_loc = p_argbuilder1(147460, { k_loc });
									R_slotdata[k_loc] = F_opcode;
									d_loc = n(-2876);
									v_loc = n(-2888);
									F_opcode = { [d_loc] = r_loc };
									r_loc = s_makeslot();
									R_slotdata[r_loc] = F_opcode;
									F_opcode = Q_argbuilder3(12055920, { r_loc, k_loc });
									d_loc = n(-2896);
									M_env[d_loc] = F_opcode;
									F_opcode = a_argbuilder2(10021170, { r_loc, k_loc });
									d_loc = n(-2891);
									P_loc = n(-2894);
									M_env[d_loc] = F_opcode;
									d_loc = n(-2895);
									F_opcode = M_env[d_loc];
									f_loc = M_env[P_loc];
									P_loc = n(-2886);
									d_loc = F_opcode(f_loc, P_loc);
									f_loc = d_loc;
									P_loc = n(-2893);
									d_loc = n(-2895);
									g_loc = n(-2878);
									F_opcode = M_env[d_loc];
									d_loc = F_opcode(f_loc, P_loc);
									P_loc = d_loc;
									d_loc = n(-2895);
									h_loc = n(-2889);
									F_opcode = M_env[d_loc];
									d_loc = F_opcode(P_loc, A_loc);
									A_loc = d_loc;
									d_loc = n(-2895);
									F_opcode = M_env[d_loc];
									u_loc = n(-2866);
									d_loc = F_opcode(A_loc, v_loc);
									v_loc = d_loc;
									d_loc = n(-2895);
									F_opcode = M_env[d_loc];
									d_loc = F_opcode(f_loc, g_loc);
									g_loc = d_loc;
									d_loc = n(-2895);
									F_opcode = M_env[d_loc];
									d_loc = F_opcode(g_loc, u_loc);
									u_loc = d_loc;
									d_loc = n(-2895);
									F_opcode = M_env[d_loc];
									d_loc = F_opcode(g_loc, h_loc);
									h_loc = d_loc;
									d_loc = n(-2904);
									F_opcode = M_env[d_loc];
									x_loc = { F_opcode(f_loc) };
									d_loc = x_loc[1];
									l_loc = x_loc[2];
									j_loc = x_loc[3];
									F_opcode = 12954508;
									x_loc = d_loc;
								end;
							end;
						else
							if F_opcode < 9507392 then
								if F_opcode < 8204141 then
									z_loc = j_loc;
									E_loc = n(-2898);
									c_loc = M_env[E_loc];
									y_loc = n(-2902);
									E_loc = n(-2882);
									O_loc = c_loc[E_loc];
									E_loc = M_env[y_loc];
									y_loc = E_loc(w_loc);
									E_loc = n(-2874);
									c_loc = O_loc(y_loc, E_loc);
									F_opcode = c_loc and 13697442 or 15140302;
									C_loc = c_loc;
								else
									F_opcode = c_loc;
									F_opcode = 15140302;
									C_loc = O_loc;
								end;
							else
								if F_opcode < 10032559 then
									I_loc = m_args[1];
									d_loc = R_slotdata[T_regs[1]];
									v_loc = n(-2890);
									A_loc = n(-2870);
									r_loc = n(-2876);
									F_opcode = d_loc[r_loc];
									d_loc = F_opcode(I_loc);
									r_loc = d_loc;
									k_loc = m_args[2];
									d_loc = n(-2906);
									F_opcode = M_env[d_loc];
									P_loc = R_slotdata[T_regs[2]];
									f_loc = P_loc[A_loc];
									P_loc = n(-2892);
									A_loc = k_loc[v_loc];
									d_loc = F_opcode(r_loc, f_loc, P_loc, A_loc);
									v_loc = n(-2870);
									d_loc = n(-2906);
									F_opcode = M_env[d_loc];
									A_loc = R_slotdata[T_regs[2]];
									P_loc = A_loc[v_loc];
									A_loc = 4;
									f_loc = P_loc + A_loc;
									P_loc = n(-2884);
									v_loc = n(-2892);
									A_loc = k_loc[P_loc];
									d_loc = F_opcode(r_loc, f_loc, v_loc, A_loc);
									d_loc = n(-2906);
									v_loc = n(-2870);
									I_loc = nil;
									F_opcode = M_env[d_loc];
									A_loc = R_slotdata[T_regs[2]];
									P_loc = A_loc[v_loc];
									A_loc = 8;
									f_loc = P_loc + A_loc;
									P_loc = n(-2892);
									v_loc = n(-2905);
									A_loc = k_loc[v_loc];
									k_loc = nil;
									d_loc = F_opcode(r_loc, f_loc, P_loc, A_loc);
									r_loc = nil;
									d_loc = {};
									F_opcode = M_env[n(-2875)];
								else
									F_opcode = 11664740;
								end;
							end;
						end;
					else
						if F_opcode < 12878871 then
							if F_opcode < 12621859 then
								if F_opcode < 11695529 then
									w_loc = nil;
									z_loc = nil;
									F_opcode = 12954508;
								else
									d_loc = R_slotdata[T_regs[1]];
									k_loc = n(-2876);
									g_loc = n(-2870);
									P_loc = n(-2870);
									I_loc = m_args[1];
									F_opcode = d_loc[k_loc];
									d_loc = F_opcode(I_loc);
									k_loc = d_loc;
									d_loc = n(-2867);
									F_opcode = M_env[d_loc];
									f_loc = R_slotdata[T_regs[2]];
									r_loc = f_loc[P_loc];
									f_loc = n(-2892);
									d_loc = F_opcode(k_loc, r_loc, f_loc);
									r_loc = d_loc;
									d_loc = n(-2867);
									v_loc = n(-2870);
									F_opcode = M_env[d_loc];
									A_loc = R_slotdata[T_regs[2]];
									P_loc = A_loc[v_loc];
									A_loc = 4;
									f_loc = P_loc + A_loc;
									P_loc = n(-2892);
									d_loc = F_opcode(k_loc, f_loc, P_loc);
									f_loc = d_loc;
									d_loc = n(-2867);
									F_opcode = M_env[d_loc];
									v_loc = R_slotdata[T_regs[2]];
									A_loc = v_loc[g_loc];
									v_loc = 8;
									P_loc = A_loc + v_loc;
									A_loc = n(-2892);
									d_loc = F_opcode(k_loc, P_loc, A_loc);
									A_loc = n(-2901);
									P_loc = d_loc;
									d_loc = n(-2908);
									v_loc = n(-2899);
									F_opcode = { [d_loc] = r_loc, [A_loc] = f_loc, [v_loc] = P_loc };
									d_loc = { F_opcode };
									F_opcode = M_env[n(-2879)];
								end;
							else
								if F_opcode < 12850642 then
									F_opcode = 8570630;
									L_loc = n(-2869);
									y_loc = M_env[L_loc];
									L_loc = y_loc(w_loc);
									y_loc = n(-2872);
									E_loc = L_loc == y_loc;
									O_loc = E_loc;
								else
									E_loc = n(-2909);
									F_opcode = 3803764;
									c_loc = M_env[E_loc];
									E_loc = c_loc(v_loc, w_loc);
									O_loc = not E_loc;
									C_loc = O_loc;
								end;
							end;
						else
							if F_opcode < 14687237 then
								if F_opcode < 13343264 then
									j_loc, w_loc = x_loc(l_loc, j_loc);
									F_opcode = j_loc and 8189690 or 851482;
								else
									L_loc = n(-2869);
									c_loc = F_opcode;
									y_loc = M_env[L_loc];
									L_loc = y_loc(w_loc);
									y_loc = n(-2877);
									E_loc = L_loc == y_loc;
									O_loc = E_loc;
									F_opcode = E_loc and 8570630 or 12826504;
								end;
							else
								if F_opcode < 15247939 then
									F_opcode = C_loc and 1805367 or 11664740;
								else
									L_loc = 10;
									E_loc = 10;
									q_loc = n(-2905);
									c_loc = n(-2890);
									C_loc = n(-2891);
									y_loc = n(-2884);
									F_opcode = M_env[C_loc];
									B_loc = 10;
									O_loc = { [c_loc] = E_loc, [y_loc] = L_loc, [q_loc] = B_loc };
									C_loc = F_opcode(w_loc, O_loc);
									F_opcode = 11224919;
								end;
							end;
						end;
					end;
				end;
				F_opcode = #Y_len;
				return o_unpack(d_loc);
			end, function(M_slots)
				local n_idx, F_slot = 1, M_slots[1];
				while F_slot do
					I_slotrefs[F_slot], n_idx = I_slotrefs[F_slot] - 1, 1 + n_idx;
					if 0 == I_slotrefs[F_slot] then
						I_slotrefs[F_slot], R_slotdata[F_slot] = nil, nil;
					end;
					F_slot = M_slots[n_idx];
				end;
			end, function(M_op, n_idx)
				local o_dispatch_table = r_builddispatch(n_idx);
				local m_dispatch_func = function(m_arg1, T_arg2, Y_arg3)
						return F_dispatcher(M_op, { m_arg1, T_arg2, Y_arg3 }, n_idx, o_dispatch_table);
					end;
				return m_dispatch_func;
			end, function(M_op, n_idx)
				local o_dispatch_table = r_builddispatch(n_idx);
				local m_dispatch_func = function(...)
						return F_dispatcher(M_op, { ... }, n_idx, o_dispatch_table);
					end;
				return m_dispatch_func;
			end, function(M_slots)
				for n_s_idx = 1, #M_slots, 1 do
					I_slotrefs[M_slots[n_s_idx]] = I_slotrefs[M_slots[n_s_idx]] + 1;
				end;
				if m_newproxy then
					local F_proxy = m_newproxy(true);
					local o_meta = Y_getmeta(F_proxy);
					o_meta[n(-2900)], o_meta[n(-2897)], o_meta[n(-2883)] = M_slots, f_const, function()
							return 748439;
						end;
					return F_proxy;
				else
					return T_setmeta({}, { [n(-2897)] = f_const, [n(-2900)] = M_slots, [n(-2883)] = function()
							return 748439;
						end });
				end;
			end, function()
				k_counter = 1 + k_counter;
				I_slotrefs[k_counter] = 1;
				return k_counter;
			end, function(M_op, n_idx)
				local o_dispatch_table = r_builddispatch(n_idx);
				local m_dispatch_func = function(m_a1, T_a2, Y_a3, t_a4, d_a5, R_a6)
						return F_dispatcher(M_op, {
							m_a1,
							T_a2,
							Y_a3,
							t_a4,
							d_a5,
							R_a6,
						}, n_idx, o_dispatch_table);
					end;
				return m_dispatch_func;
			end;
		return (P_callslot(5389476, {}))(o_unpack(d_arg_varargs));
	end)(getfenv and getfenv() or _ENV, unpack or table[n(-2881)], newproxy, setmetatable, getmetatable, select, varargs);
end)(...);
