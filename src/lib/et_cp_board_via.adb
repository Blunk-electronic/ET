------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                    COMMAND PROCESSOR / BOARD / VIA                       --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2026                                                -- 
-- Mario Blunk / Blunk electronic                                           --
-- Buchfinkenweg 3 / 99097 Erfurt / Germany                                 --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

--   For correct displaying set tab with in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
-- To Do:
-- - rework
-- - propose arguments if command incomplete
--

with ada.text_io;						use ada.text_io;
with ada.characters.handling;			use ada.characters.handling;
with ada.strings; 						use ada.strings;

with et_exceptions;						use et_exceptions;
with et_modes.board;					use et_modes.board;
with et_keywords;						use et_keywords;
with et_module;							use et_module;
with et_module_names;					use et_module_names;
with et_pcb_stack;						use et_pcb_stack;
with et_pcb_signal_layers;				use et_pcb_signal_layers;
with et_board_geometry;					use et_board_geometry;
with et_board_ops;						use et_board_ops;
with et_board_ops_vias;
with et_board_ops_signal_layers;		use et_board_ops_signal_layers;
with et_module_board_user_settings;		use et_module_board_user_settings;
with et_board_ops_user_settings;		use et_board_ops_user_settings;
with et_net_names;
with et_drills;
with et_design_rules_board;
with et_vias;



package body et_cp_board_via is

	use pac_generic_modules;
	use pac_geometry_2;
	use pac_contours;


	
	
	procedure set_via_properties (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);


		-- CS move to et_keywords:
		kw_drill	: constant string := "drill";
		kw_restring	: constant string := "restring";
		kw_inner	: constant string := "inner";
		kw_outer	: constant string := "outer";
		kw_dru		: constant string := "dru";

		
		procedure expect_keywords is begin
			raise syntax_error_1 with 
				"ERROR: Expect keyword "
				& enclose_in_quotes (kw_drill) & " or "
				& enclose_in_quotes (kw_restring) 
				& " after " & enclose_in_quotes (to_lower (to_string (noun))) & " !";
		end expect_keywords;

		

		
		procedure deactivate_drill (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is begin
			module.board.user_settings.vias.drill.active := false;
		end deactivate_drill;

		
		procedure activate_drill (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is begin
			module.board.user_settings.vias.drill.active := true;
			module.board.user_settings.vias.drill.size := to_distance (get_field (cmd, 6));
		end activate_drill;

		
		procedure deactivate_inner_restring (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is begin
			module.board.user_settings.vias.restring_inner.active := false;
		end deactivate_inner_restring;

		
		procedure activate_inner_restring (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is begin
			module.board.user_settings.vias.restring_inner.active := true;
			module.board.user_settings.vias.restring_inner.width := to_distance (get_field (cmd, 7));
		end activate_inner_restring;

		
		procedure deactivate_outer_restring (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is begin
			module.board.user_settings.vias.restring_outer.active := false;
		end deactivate_outer_restring;

		
		procedure activate_outer_restring (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is begin
			module.board.user_settings.vias.restring_outer.active := true;
			module.board.user_settings.vias.restring_outer.width := to_distance (get_field (cmd, 7));
		end activate_outer_restring;


		
	begin -- set_via_properties
		-- CS log message
		
		case cmd_field_count is			
			when 6 => 
				-- board demo set via drill 0.3/dru
				if get_field (cmd, 5) = kw_drill then
					if get_field (cmd, 6) = kw_dru then
						update_element (generic_modules, module, deactivate_drill'access);
					else
						update_element (generic_modules, module, activate_drill'access);

						-- CS validate against dru settings
					end if;
				else
					expect_keywords;
				end if;

				
			when 7 => 
				-- board demo set via restring inner/outer 0.2
				if get_field (cmd, 5) = kw_restring then

					-- board demo set via restring inner 0.2
					-- board demo set via restring inner dru
					if get_field (cmd, 6) = kw_inner then
						if get_field (cmd, 7) = kw_dru then
							update_element (generic_modules, module, deactivate_inner_restring'access);
						else
							update_element (generic_modules, module, activate_inner_restring'access);
							
							-- CS validate against dru settings	
						end if;

					-- board demo set via restring outer 0.2
					-- board demo set via restring outer dru
					elsif get_field (cmd, 6) = kw_outer then
						if get_field (cmd, 7) = kw_dru then
							update_element (generic_modules, module, deactivate_outer_restring'access);
						else
							update_element (generic_modules, module, activate_outer_restring'access);

							-- CS validate against dru settings
						end if;
						
					else
						raise syntax_error_1 with
							"ERROR: Expect keywords " 
							& enclose_in_quotes (kw_inner) & " or "
							& enclose_in_quotes (kw_outer) 
							& " after keyword " & enclose_in_quotes (kw_restring) & " !";
					
					end if;
				else
					expect_keywords;
				end if;
				
				
			when 8 .. type_field_count'last => 
				command_too_long (cmd, cmd_field_count - 1);

			when others => command_incomplete (cmd);
		end case;

	end set_via_properties;


		




	

	procedure place_via (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		use et_net_names;
		use et_drills;
		use et_vias;
		use et_design_rules_board;
		use et_board_ops_vias;
		
		
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);


		net_name		: pac_net_name.bounded_string;
		drill			: type_drill;
		restring_outer	: type_restring_width;
		restring_top	: type_restring_width;
		restring_bottom	: type_restring_width;
		restring_inner	: type_restring_width;
		buried_layers	: type_buried_layers;
		lower_layer		: type_via_layer;
		upper_layer		: type_via_layer;

		
		procedure set_net_name is begin
			-- CS check net name: characters, length, existence of net
			net_name := to_net_name (get_field (cmd, 5));
		end set_net_name;

		
		procedure set_position is begin
			drill.position := to_vector_model (get_field (cmd, 6), get_field (cmd, 7));

			-- CS check position: must be inside board area
		end set_position;


		-- CS move to et_keywords:
		keyword_buried	: constant string := "buried";
		keyword_blind	: constant string := "blind";
		keyword_top		: constant string := "top";
		keyword_bottom	: constant string := "bottom";

		
		procedure through is
			via : type_via (THROUGH);
		begin
			via := (drill with
				category		=> THROUGH,
				restring_inner	=> restring_inner,
				restring_outer	=> restring_outer);
					
			et_board_ops_vias.place_via (module, net_name, via, log_threshold + 1);
		end through;

		
		procedure blind_top is
			via : type_via (BLIND_DRILLED_FROM_TOP);
		begin
			via := (drill with
				category		=> BLIND_DRILLED_FROM_TOP,
				restring_inner	=> restring_inner,
				restring_top	=> restring_top,
				lower			=> lower_layer);
					
			et_board_ops_vias.place_via (module, net_name, via, log_threshold + 1);
		end blind_top;

		
		procedure blind_bottom is
			via : type_via (BLIND_DRILLED_FROM_BOTTOM);
		begin
			via := (drill with
				category		=> BLIND_DRILLED_FROM_BOTTOM,
				restring_inner	=> restring_inner,
				restring_bottom	=> restring_bottom,
				upper			=> upper_layer);
					
			et_board_ops_vias.place_via (module, net_name, via, log_threshold + 1);
		end blind_bottom;

		
		procedure buried is
			via : type_via (BURIED);
		begin
			via := (drill with
				category		=> BURIED,
				restring_inner	=> restring_inner,
				layers			=> buried_layers);
					
			et_board_ops_vias.place_via (module, net_name, via, log_threshold + 1);
		end buried;

		
		rules : constant type_design_rules_board := get_pcb_design_rules (module);

		-- get the user specific settings of the board
		settings : constant type_user_settings := 
			get_user_settings (module);


		
	begin -- place_via
		-- CS log message

		
		-- Set the drill size and restring according to a user specific values:
		-- If user has not specified defaults, use values given in DRU data set:

		-- set drill size:
		if settings.vias.drill.active then
			drill.diameter	:= settings.vias.drill.size;
		else
			drill.diameter	:= rules.sizes.drills;
		end if;

		-- CS: take minimum drill diameter as defined in net class into account
		-- Requres a command like "set via drill class"

		
		-- set outer restring:
		if settings.vias.restring_outer.active then
			restring_outer	:= settings.vias.restring_outer.width;
		else
			restring_outer	:= auto_set_restring (OUTER, drill.diameter);
		end if;
		
		restring_top	:= restring_outer; -- for blind via drilled from top
		restring_bottom	:= restring_outer; -- for blind via drilled from bottom

		
		-- set inner restring:
		if settings.vias.restring_inner.active then
			restring_inner	:= settings.vias.restring_inner.width;
		else
			restring_inner	:= auto_set_restring (INNER, drill.diameter, rules.sizes.restring.delta_size);
		end if;

		
		case cmd_field_count is
			when 7 => 
				-- example: board demo place via RESET_N 10 14
				set_net_name;
				set_position;
				through;

				
			when 10 =>				
				if get_field (cmd, 8) = keyword_buried then
					-- example: board demo place via RESET_N 10 14 buried 2 15					
					set_net_name;
					set_position;
					buried_layers := to_buried_layers (
								upper	=> get_field (cmd, 9), 
								lower	=> get_field (cmd, 10),
								bottom	=> get_deepest_conductor_layer (module));
					buried;

					
				elsif get_field (cmd, 8) = keyword_blind then
					-- example: board demo place via RESET_N 10 14 blind top 5
					-- example: board demo place via RESET_N 10 14 blind bottom 2
					set_net_name;
					set_position;

					if get_field (cmd, 9) = keyword_top then
						lower_layer := to_signal_layer (get_field (cmd, 10));
						blind_top;
						
					elsif get_field (cmd, 9) = keyword_bottom then
						upper_layer := to_signal_layer (get_field (cmd, 10));
						blind_bottom;
						
					else
						raise syntax_error_1 with 
							"ERROR: Expect keywords " 
							& enclose_in_quotes (keyword_top)
							& " or " 
							& enclose_in_quotes (keyword_bottom)
							& " after keyword " 
							& enclose_in_quotes (keyword_blind)
							& " !";							
					end if;
						
				else
					raise syntax_error_1 with 
						"ERROR: Expect keywords " & enclose_in_quotes (keyword_blind)
						& " or " & enclose_in_quotes (keyword_buried)
						& " after y position !";
				end if;

				
			when 11 .. type_field_count'last => 
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;

	end place_via;





	procedure move_via (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is begin
		null;
		-- CS
	end move_via;

	



	procedure delete_via (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is begin
		null;
		-- CS
	end delete_via;


	

	
	
end et_cp_board_via;
	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
