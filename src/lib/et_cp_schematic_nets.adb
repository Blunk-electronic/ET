------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                COMMAND PROCESSOR / SCHEMATIC / NETS                      --
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

--   For correct displaying set tab with in your editor to 4.

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
-- - set exit code if targeted object does not exist
--


with ada.text_io;						use ada.text_io;
with ada.characters.handling;			use ada.characters.handling;
with ada.strings; 						use ada.strings;

with et_runmode;						use et_runmode;

with et_sheets;							use et_sheets;
with et_schematic_coordinates;			use et_schematic_coordinates;
with et_schematic_geometry;				use et_schematic_geometry;
with et_coordinates_abs_rel;			use et_coordinates_abs_rel;

with et_net_class_name;
with et_net_connectors;					use et_net_connectors;
with et_net_scope;						use et_net_scope;
with et_netlist_category;				use et_netlist_category;
with et_schematic_ops_netlists_2;
with et_net_names;						use et_net_names;
with et_schematic_ops_nets;				use et_schematic_ops_nets;

with et_schematic_ops_groups;
with et_board_ops_groups;

with et_assembly_variant_name;
with et_canvas_schematic;
with et_board_ops_net_class;
with et_canvas_schematic_nets;
with et_canvas_schematic_preliminary_object;


package body et_cp_schematic_nets is

	use pac_generic_modules;
	use pac_geometry_2;


	
	procedure export_netlist (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		use et_schematic_ops_netlists_2;
		use et_assembly_variant_name;
		
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);
		
		category : type_netlist_category := type_netlist_category'first;
		
	begin
		-- CS log message

		-- CS test whether given variant exists

		case cmd_field_count is
			when 6 =>
				-- schematic demo make netlist low_cost 1
				
				category := to_netlist_category (get_field (cmd, 6));
				
				case category is
					when NETLIST_CAT_1 =>
					
						make_netlist_cat_1 (
							module_cursor 	=> module,
							variant			=> to_variant (get_field (cmd, 5)),
							log_threshold	=> log_threshold + 1);

					when NETLIST_CAT_2 =>
						null;
						-- CS
				end case;
				
				
				
							
			when 7 .. type_field_count'last =>
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;
	end export_netlist;











	procedure set_net_scope (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is		
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		
		
		
		procedure do_it is
			net_name : pac_net_name.bounded_string;
			scope : type_net_scope;
		begin
			net_name := to_net_name (get_field (cmd, 5));
			scope := to_net_scope (get_field (cmd, 6));

			-- Proceed if the net exists:
			if net_exists (module, net_name) then
			
				set_scope (
					module_cursor 	=> module,
					net_name		=> net_name,
					scope			=> scope,
					log_threshold	=> log_threshold + 1);

			else
				message_net_not_found (SEVERITY_ERROR, net_name);
			end if;
		end do_it;
		
		
	begin
		log (text => "set net scope", level => log_threshold);
		log_indentation_up;

		case cmd_field_count is
			when 6 =>
				do_it;

			when 7 .. type_field_count'last =>
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;		

		log_indentation_down;
	end set_net_scope;




	




	procedure show_net (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		

		
		
		procedure do_it is
			net_name : pac_net_name.bounded_string;
		begin
			net_name := to_net_name (get_field (cmd, 5));

			-- Proceed if the net exists:
			if net_exists (module, net_name) then
				show_net (module, net_name, log_threshold + 1);
			else
				message_net_not_found (SEVERITY_ERROR, net_name);
			end if;
		end do_it;

		
		
		
		procedure preprocess_command is begin
			case cmd_field_count is
				when 5 => 
					do_it;
				
				when 6 .. type_field_count'last =>
					command_too_long (cmd, cmd_field_count - 1);
				
				when others => command_incomplete (cmd);
			end case;		
		end preprocess_command;
		
		
	begin
		log (text => "show net", level => log_threshold);
		log_indentation_up;		
		
		
		-- Show operations are only useful and possible in graphical
		-- runmode. So we start preprocessing the given command
		-- only in graphical runmode:
		case runmode is
			when MODE_MODULE =>
			
				-- Deselect all objects in the schematic
				-- and board drawing. This is required in case
				-- the specified net does not exist. 
				-- It is redundant in case the specified net
				-- does exist. The reset would be executed twice,
				-- the first time here and the second time
				-- by procedure show_net in package et_schematic_ops_nets:
				et_schematic_ops_groups.reset_objects (
					module, log_threshold + 1);
					
				et_board_ops_groups.reset_objects (
					module, log_threshold + 1);

				preprocess_command;

				
			when others =>
				skipped_in_this_runmode (log_threshold + 1);
					
		end case;			

		
		log_indentation_down;
	end show_net;


	
	
	
	
	
	
	
	
	procedure place_net_connector (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		
	begin
		log (text => "place net connector", level => log_threshold);
		log_indentation_up;


		case cmd_field_count is
			when 8 =>

				place_net_connector (
					module_cursor	=> module,
					position		=> to_position (
											point => type_vector_model (set (
												x => to_distance (get_field (cmd, 6)),
												y => to_distance (get_field (cmd, 7)))),
											sheet => to_sheet (get_field (cmd, 5))), -- sheet number
	
					-- A connector requires specification of signal direction:
					direction		=> to_direction (get_field (cmd, 8)), -- INPUT, OUTPUT, PASSIVE, ...
					log_threshold	=> log_threshold + 1);

				
			when 9 .. type_field_count'last =>
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;
		
		
		log_indentation_down;
	end place_net_connector;
	
	
	
	
	
	
	
	
		

	procedure delete_net_connector (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		
	begin
		log (text => "delete net connector", level => log_threshold);
		log_indentation_up;

		-- CS
		null;
		
		log_indentation_down;
	end delete_net_connector;


	
	
	
	

	procedure place_net_label (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		
	begin
		log (text => "place net label", level => log_threshold);
		log_indentation_up;


		case cmd_field_count is
			when 7 =>
				
				place_net_label (
					module_cursor	=> module,
					position		=> to_position (
											point => type_vector_model (set (
												x => to_distance (get_field (cmd, 6)),
												y => to_distance (get_field (cmd, 7)))),
											sheet => to_sheet (get_field (cmd, 5))), -- sheet number
	
					log_threshold	=> log_threshold + 1);

				
			when 8 .. type_field_count'last =>
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;
		
		
		log_indentation_down;
	end place_net_label;

		
		
		
		

		

	procedure delete_net_label (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		
	begin
		log (text => "delete net label", level => log_threshold);
		log_indentation_up;


		case cmd_field_count is
			when 7 =>
				delete_net_label
					(
					module_cursor	=> module,

					position		=> to_position (
										point => type_vector_model (set (
											x => to_distance (get_field (cmd, 6)),
											y => to_distance (get_field (cmd, 7)))),
										sheet => to_sheet (get_field (cmd, 5))), -- sheet number
					
					log_threshold	=> log_threshold + 1);
				
			when 8 .. type_field_count'last =>
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;
		
		
		log_indentation_down;
	end delete_net_label;
	
	
	
	
	
	
		
		

	procedure move_net_label (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		
	begin
		log (text => "move net label", level => log_threshold);
		log_indentation_up;

		-- CS
		null;
		
		log_indentation_down;
	end move_net_label;
	
	
	
	
	
	
	

	procedure set_net_class (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);
		
		
		procedure do_it is
			use et_board_ops_net_class;
			use et_net_class_name;
			net_name : pac_net_name.bounded_string;
			net_class : pac_net_class_name.bounded_string;
		begin
			net_name := to_net_name (get_field (cmd, 5));
			net_class := to_net_class_name (get_field (cmd, 6));

			-- Proceed if net exists:
			if net_exists (module, net_name) then
			
			-- CS Test if the net class exists.
			
				set_net_class (
					module_cursor	=> module,
					net_name		=> net_name,
					net_class		=> net_class,
					log_threshold	=> log_threshold + 1);

			else
				message_net_not_found (SEVERITY_ERROR, net_name);
			end if;
		end do_it;
		
		
	begin
		log (text => "set net class", level => log_threshold);
		log_indentation_up;

		case cmd_field_count is
			when 6 =>
				do_it;
				
			when 7 .. type_field_count'last =>
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;
		
		log_indentation_down;
	end set_net_class;
		
		
		

		
		
		
		

	procedure draw_net (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		

		use et_canvas_schematic.pac_canvas;
		use et_canvas_schematic_nets;
		use et_canvas_schematic_preliminary_object;
		
		
		procedure no_name_given is begin
			-- If this is the first net right after system start,
			-- then an anonymous net will be used.
			-- Otherwise the net name used last will be applied
			-- as it is stored in object_net_name.
			if is_empty (object_net_name) then -- after system start
				object_net_name := get_lowest_available_anonymous_net (module);
				log (text => "apply anonymous name: " 
					& net_name_to_string (object_net_name),
					level => log_threshold + 2);
			else
				log (text => "apply name used last: " 
					& net_name_to_string (object_net_name),
					level => log_threshold + 2);
			end if;

			set_status (status_draw_net & " of net " 
				& net_name_to_string (object_net_name));
		end;

		
		procedure explicit_name_given is
			name_s : constant string := get_field (cmd, 5); -- RESET_N
			name_b : pac_net_name.bounded_string;
		begin
			-- Validate the given net name:
			check_net_name_length (name_s);
			name_b := to_net_name (name_s);
			check_net_name_characters (name_b);

			-- Assign the net name:
			object_net_name := name_b;

			set_status (status_draw_net & " of net " 
				& net_name_to_string (object_net_name));
		end explicit_name_given;


		
		procedure segment_given is
			name_s : constant string := get_field (cmd, 5); -- RESET_N
			name_b : pac_net_name.bounded_string;

			A : type_object_position; -- start point of segment
			B : type_vector_model; -- end point of segment
		begin
			-- Validate the given net name:
			check_net_name_length (name_s);
			name_b := to_net_name (name_s);
			check_net_name_characters (name_b);

			-- Assign the net name:
			object_net_name := name_b;

			-- Assign the start and end of the segment:
			A := to_position (
				point => to_vector_model (get_field (cmd, 7), get_field (cmd, 8)), -- x/y
				sheet => to_sheet (get_field (cmd, 6))); -- sheet number

			B := to_vector_model (get_field (cmd, 9), get_field (cmd, 10)); -- x/y

			-- Insert the net segment in the database:
			insert_net_segment (
				module_cursor	=> module,
				net_name		=> object_net_name,
				A				=> A,					
				B 				=> B,					
				log_threshold	=> log_threshold + 1);
		end segment_given;
		
		
	begin	
		log (text => "draw net", level => log_threshold);
		log_indentation_up;

	
		case get_origin (cmd) is
			when ORIGIN_CONSOLE =>

				-- The command may contain more or less arguments
				-- and can still be valid.
				-- However a minimum of arguments must be ensured
				-- and a maximum must not be exceeded:
				case cmd_field_count is
					when 4 => -- like "draw net"
						log (text => "no name given", level => log_threshold + 1);
						log_indentation_up;
						no_name_given;
						log_indentation_down;
						
					when 5 => -- like "draw net RESET_N"
						explicit_name_given;
					
					when 10 => -- like "draw net RESET_N 1 90 100  100 100"
						segment_given;

					when 11 .. type_field_count'last => 
						command_too_long (cmd, cmd_field_count - 1);
					
					when others =>
						command_incomplete (cmd);
				end case;

				
				
			when ORIGIN_SCRIPT =>

				-- The command MUST contain a certain number of
				-- arguments:
				case cmd_field_count is
					when 10 => -- like "draw net RESET_N 1 90 100  100 100"
						segment_given;

					when 11 .. type_field_count'last => 
						command_too_long (cmd, cmd_field_count - 1);
						
					when others => 
						command_incomplete (cmd);
						
				end case;

		end case;
		
		log_indentation_down;

		-- CS exception handler
		-- CS set_exit_code (cmd, 3);
	end draw_net;
		
		
		
		
		
		
		
		
	
	procedure delete_net (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		
		
		
		-- This procedure deletes the net on all sheets:
		procedure delete_all is
			net_name : pac_net_name.bounded_string;
		begin
			net_name := to_net_name (get_field (cmd, 5)); -- RESET_N
			
			if net_exists (module, net_name) then

				delete_net (
					module_cursor		=> module,
					net_name			=> net_name,
					sheet				=> 1, -- no meaning
					all_sheets			=> TRUE,
					log_threshold		=> log_threshold + 1);
		
			else	
				message_net_not_found (SEVERITY_ERROR, net_name);
			end if;
		end delete_all;
		
		
		
		-- This procedure deletes the net on a single sheet:
		procedure delete_sheet is
			net_name : pac_net_name.bounded_string;
			sheet : type_sheet;
		begin
			net_name := to_net_name (get_field (cmd, 5)); -- RESET_N
			sheet := to_sheet (get_field (cmd, 6));
			
			if net_exists (module, net_name) then

				delete_net (
					module_cursor		=> module,
					net_name			=> net_name,
					sheet				=> sheet,
					log_threshold		=> log_threshold + 1);
		
			else	
				message_net_not_found (SEVERITY_ERROR, net_name);
			end if;		
		end delete_sheet;
		
		
	begin
		log (text => "delete net", level => log_threshold);
		log_indentation_up;
		
		case cmd_field_count is
			when 5 =>
				-- example 1: "delete net RESET_N"
				delete_all;

			when 6 =>
				-- example 2: "delete net RESET_N 2"
				delete_sheet;
			
			when 7 .. type_field_count'last =>
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;
		
		log_indentation_down;
	end delete_net;
		

		
		
		
		
		
		
		
	procedure rename_net (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		

		net_name_before, net_name_after : pac_net_name.bounded_string;
		sheet : type_sheet;
		catch_zone : type_catch_zone;
		
		
		-- This procedure renames the net on all sheets:
		procedure rename_all is begin
			net_name_before := to_net_name (get_field (cmd, 5)); -- RESET
			net_name_after  := to_net_name (get_field (cmd, 6)); -- RESET_N
		
			if net_exists (module, net_name_before) then
			
				rename_net (
					module_cursor		=> module,
					net_name_before		=> net_name_before,
					net_name_after		=> net_name_after,
					all_sheets			=> true,
					log_threshold		=> log_threshold + 1);
		
			else
				message_net_not_found (SEVERITY_ERROR, net_name_before);
			end if;
		end rename_all;
		

		
		-- This procedure renames the net on the specified sheet:
		procedure rename_on_sheet is begin
			net_name_before := to_net_name (get_field (cmd, 5)); -- RESET
			net_name_after  := to_net_name (get_field (cmd, 6)); -- RESET_N
			sheet := to_sheet (get_field (cmd, 7)); -- 2
			
			if net_exists (module, net_name_before) then
			
				-- CS: Test if the given sheet exists.
				
				rename_net (
					module_cursor		=> module,
					net_name_before		=> net_name_before,
					net_name_after		=> net_name_after,
					sheet				=> sheet,
					log_threshold		=> log_threshold + 1);
		
			else
				message_net_not_found (SEVERITY_ERROR, net_name_before);
			end if;
		end rename_on_sheet;

		
		
		-- This procedure renames a strand on a given sheet:
		procedure rename_strand is begin
			net_name_before := to_net_name (get_field (cmd, 5)); -- RESET
			net_name_after  := to_net_name (get_field (cmd, 6)); -- RESET_N
			sheet := to_sheet (get_field (cmd, 7)); -- 2
		
			catch_zone := set_catch_zone (
				center	=> to_vector_model (get_field (cmd, 8), get_field (cmd, 9)),
				radius	=> to_zone_radius (get_field (cmd, 10))); -- 50 90 5

			if net_exists (module, net_name_before) then
			
				-- CS: Test if the given sheet exist.
				
				rename_strand (
					module_cursor		=> module,
					net_name_before		=> net_name_before,
					net_name_after		=> net_name_after,
					sheet				=> sheet,
					catch_zone			=> catch_zone,
					log_threshold		=> log_threshold + 1);
		
			else
				message_net_not_found (SEVERITY_ERROR, net_name_before);
			end if;
		end rename_strand;
		
		
		
	begin
		log (text => "rename net", level => log_threshold);
		log_indentation_up;

		case cmd_field_count is

			when 6 =>
				-- example: rename net RESET_N RST_N
				rename_all;
				
			when 7 =>
				-- example: rename net RESET_N RST_N 2
				rename_on_sheet;
				
			when 10 =>
				-- example: rename net RESET_N RST_N 2 50 90 5
				rename_strand;
				
			when 11 .. type_field_count'last =>
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;
		
		log_indentation_down;
	end rename_net;

		
		
		
		

		
	
	
	procedure delete_net_segment (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		
		
		catch_zone : type_catch_zone;
	begin
		log (text => "delete net segment", level => log_threshold);
		log_indentation_up;

		
		case cmd_field_count is
			-- example: "delete segment 1 97 99 2"
			when 8 =>
				catch_zone := set_catch_zone (
					center	=> to_vector_model (get_field (cmd, 6), get_field (cmd, 7)),
					radius	=> to_zone_radius (get_field (cmd, 8)));
				
				delete_segment (
					module_cursor	=> module,
					sheet			=> to_sheet (get_field (cmd, 5)),
					catch_zone		=> catch_zone,
					log_threshold	=> log_threshold + 1);

			when 9 .. type_field_count'last =>
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;
		
		
		log_indentation_down;
	end delete_net_segment;
	
	
	
	
	

	
	
	
		
	
	procedure drag_net_segment (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		
		
		catch_zone : type_catch_zone;
	begin
		log (text => "drag net segment", level => log_threshold);
		log_indentation_up;

		
		-- example: "drag segment 1 80 100 2 relative 10 0"
		case cmd_field_count is
			when 11 =>

				catch_zone := set_catch_zone (
					center	=> to_vector_model (get_field (cmd, 6), get_field (cmd, 7)),
					radius	=> to_zone_radius (get_field (cmd, 8)));
				
				drag_segment (
					module_cursor	=> module,
					sheet			=> to_sheet (get_field (cmd, 5)),
					catch_zone		=> catch_zone,					
					coordinates		=> to_coordinates (get_field (cmd, 9)), -- relative/absolute					
					destination		=> to_vector_model (get_field (cmd, 10), get_field (cmd, 11)),
					log_threshold	=> log_threshold + 1);
					

			when 13 .. type_field_count'last =>
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;
		
		
		log_indentation_down;
	end drag_net_segment;


	
	
	

	
	
	procedure delete_net_strand (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);		
		
		catch_zone : type_catch_zone;
	begin
		log (text => "delete net strand", level => log_threshold);
		log_indentation_up;


		case cmd_field_count is
			-- example: "delete strand 1 97 99 2"
			when 8 =>
				catch_zone := set_catch_zone (
					center	=> to_vector_model (get_field (cmd, 6), get_field (cmd, 7)),
					radius	=> to_zone_radius (get_field (cmd, 8)));
				
				delete_strand (
					module_cursor	=> module,
					sheet			=> to_sheet (get_field (cmd, 5)),
					catch_zone		=> catch_zone,
					log_threshold	=> log_threshold + 1);

			when 9 .. type_field_count'last => 
				command_too_long (cmd, cmd_field_count - 1);
				
			when others => command_incomplete (cmd);
		end case;
		
		
		log_indentation_down;
	end delete_net_strand;

		
		
	
end et_cp_schematic_nets;


	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
