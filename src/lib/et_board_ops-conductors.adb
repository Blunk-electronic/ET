------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                     BOARD OPERATIONS / CONDUCTOR OBJECTS                 --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                --
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

with ada.tags;

with et_mirroring;					use et_mirroring;
with et_schematic_ops.nets;			use et_schematic_ops.nets;
with et_schematic_ops;				use et_schematic_ops;
with et_board_ops.devices;			use et_board_ops.devices;
with et_board_ops.ratsnest;			use et_board_ops.ratsnest;

with et_fill_zones.boards;			use et_fill_zones.boards;

with et_devices_electrical;


package body et_board_ops.conductors is

	use pac_nets;

	

	function is_freetrack (
		net_name : in pac_net_name.bounded_string) 
		return boolean 
	is 
		use pac_net_name;
	begin
		if length (net_name) = 0 then
			return true;
		else
			return false;
		end if;
	end is_freetrack;


	
	
	function freetrack (
		net_name : in pac_net_name.bounded_string) 
		return string 
	is 
		use pac_net_name;
	begin
		if length (net_name) = 0 then
			return " freetrack";
		else
			return " net " & enclose_in_quotes (to_string (net_name));
		end if;
	end freetrack;


	

	procedure no_net_segment_found (
		layer		: in et_pcb_stack.type_signal_layer;
		zone		: in type_catch_zone) 
	is begin
		log (importance => WARNING, text => "no net segment found in layer" 
			& to_string (layer) 
			& " in" & to_string (zone));
	end no_net_segment_found;

	
	

	-- If the terminal is a THT type, then the track may start at any signal layer.
	-- If the terminal is an SMT type, then the track may start at either the top or bottom
	-- signal layer. If operator indeed whishes an inner layer a warning must be issued.
	procedure check_terminal_face_vs_layer (
		module_cursor	: in pac_generic_modules.cursor;											   
		terminal		: in type_terminal_position_fine;
		layer			: in et_pcb_stack.type_signal_layer) 
	is
		procedure warning is begin
			log (WARNING, "The terminal is an SMT type. Via required to connect with inner layer !");
		end;
		
	begin
		-- If terminal is SMT type: check desired layer against terminal.face 
		-- and issue warning if layer is an inner layer.
		if terminal.technology = SMT then

			if terminal.face = TOP then
				if layer /= type_signal_layer'first then
					warning;
				end if;

			else -- terminal.face is bottom
				if layer /= get_layer_count (module_cursor) then
					warning;
				end if;
			end if;

		end if;
	end check_terminal_face_vs_layer;



	
	
	procedure add_line (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in pac_net_name.bounded_string; -- reset_n
		line			: in type_conductor_line;
		log_threshold	: in type_log_level)
	is
		
		procedure do_it (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			-- A track belonging to a net requires the net to be located in the given module:
			net_cursor : pac_nets.cursor := find (module.nets, net_name);

			use et_nets;
			
			procedure add (
			-- Appends the track to the net.
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net) 
			is
				use pac_conductor_lines;
			begin
				append (
					container	=> net.route.lines,
					new_item	=> line);
			end add;

		begin -- add_named_track
			if net_exists (net_cursor) then
				
				pac_nets.update_element (
					container	=> module.nets,
					position	=> net_cursor,
					process		=> add'access);
				
			else
				net_not_found (net_name);
			end if;

		end do_it;

		
	begin
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> do_it'access);

		update_ratsnest (module_cursor, log_threshold + 1);
	end add_line;
	


	
	
	procedure add_line (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in pac_net_name.bounded_string := et_net_names.no_name; -- reset_n
		line			: in type_conductor_line;
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		use pac_conductor_lines;
		
		procedure add_freetrack (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is begin
			append (
				container	=> module.board.conductors_floating.lines,
				new_item	=> line);
		end;
		
	begin
		log (text => "module " & to_string (module_name) &
			freetrack (net_name) &
			" drawing " & to_string (line, true),  -- log incl. width
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);

		-- make sure the desired layer is available according to current layer stack:
		test_layer (module_cursor, line.layer);
		
		if is_freetrack (net_name) then
			
			update_element (
				container	=> generic_modules,
				position	=> module_cursor,
				process		=> add_freetrack'access);

		else
			add_line (module_cursor, net_name, line, log_threshold + 1);
		end if;

	end add_line;

	

	
	--procedure add_line (
		--module_cursor	: in pac_generic_modules.cursor;
		--net_cursor		: in pac_nets.cursor; -- reset_n
		--line			: in type_conductor_line;
		--log_threshold	: in type_log_level) 
	--is

		--procedure add_named_track (
			--module_name	: in pac_module_name.bounded_string;
			--module		: in out type_generic_module) 
		--is
			--use et_nets;
			
			--procedure add (
			---- Appends the track to the net.
				--net_name	: in pac_net_name.bounded_string;
				--net			: in out type_net) 
			--is
				--use pac_conductor_lines;
			--begin
				--append (
					--container	=> net.route.lines,
					--new_item	=> line);
			--end add;

		--begin -- add_named_track
			--pac_nets.update_element (
				--container	=> module.nets,
				--position	=> net_cursor,
				--process		=> add'access);
		--end add_named_track;

	--begin -- draw_track_line
		--update_element (
			--container	=> generic_modules,
			--position	=> module_cursor,
			--process		=> add_named_track'access);

	--end add_line;


	
	procedure add_line (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in pac_net_name.bounded_string; -- reset_n
		layer			: in et_pcb_stack.type_signal_layer;
		width			: in type_track_width;
		device			: in type_device_name;
		terminal		: in pac_terminal_name.bounded_string;
		direction		: in type_rotation_model;
		length			: in type_distance_positive;
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		-- This is going to be the segment we will insert. In the follwing it
		-- will be tailored according to given terminal position, direction and length.
		-- Finally it will be added to the list of line segments (via procedure add_named_track)
		-- to the given net.
		line : type_conductor_line;

		use et_devices_electrical;		
		device_cursor : pac_devices_sch.cursor;

		
		procedure make_line (terminal_position : in type_terminal_position_fine) is begin

			-- Build the start point of the line:
			-- The start point of the line is always the x/y of the terminal.
			-- further-on set line width and layer.
			line := (
				A	=> to_point (terminal_position.place),
				width		=> width, -- as given by operator
				layer		=> layer, -- as given by operator
				others 		=> <>);

			check_terminal_face_vs_layer (module_cursor, terminal_position, layer);
			
			-- Build the end point of the line. It is the start point moved in direction at given length:
			--line.B := type_vector_model (move (
					--point 		=> terminal_position.place,
					--direction	=> direction,
					--distance	=> length));
			line.B := move (
					point 		=> line.A,
					direction	=> direction,
					distance	=> length);
			
		end make_line;


	begin
		log (text => "module " & to_string (module_name) &
			" " & to_string (net_name) &
			" drawing line in layer" & to_string (layer) &
			" from " & to_string (device) & " terminal " & to_string (terminal) &
			" direction " & to_string (direction) & " length " & to_string (length),
			level => log_threshold);

		-- locate module and device
		module_cursor := locate_module (module_name);
		device_cursor := locate_device (module_cursor, device);
		-- CS call procedure device_not_found if
		-- device_cursor is no_element ?
		
		-- make sure the desired layer is available according to current layer stack:
		test_layer (module_cursor, layer);
		
		make_line (get_terminal_position (module_cursor, device_cursor, terminal));

		add_line (module_cursor, net_name, line, log_threshold + 1);
	end add_line;



	
	
	procedure add_line (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in pac_net_name.bounded_string; -- reset_n
		layer			: in et_pcb_stack.type_signal_layer;
		width			: in type_track_width;
		device			: in type_device_name;
		terminal		: in pac_terminal_name.bounded_string;
		direction		: in type_rotation_model;
		axis			: in type_axis_2d;
		notches			: in type_grid_notches;
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		-- This is going to be the segment we will insert. In the follwing it
		-- will be tailored according to given terminal position, direction, axis and grid notches.
		-- Finally it will be added to the list of line segments (via procedure add_named_track)
		-- to the given net.
		line : type_conductor_line;

		use et_devices_electrical;		
		device_cursor : pac_devices_sch.cursor;

		
		procedure make_line (terminal_position : in type_terminal_position_fine) is begin

			-- Build the start point of the line:
			-- The start point of the line is always the x/y of the terminal.
			-- further-on set line width and layer.
			line := (
				A	=> to_point (terminal_position.place),
				width		=> width, -- as given by operator
				layer		=> layer, -- as given by operator
				others 		=> <>);

			check_terminal_face_vs_layer (module_cursor, terminal_position, layer);
			
			-- Build the end point of the line. It is the start point moved in direction:
			-- CS
			
		end make_line;


	begin
		log (text => "module " & to_string (module_name) &
			" " & to_string (net_name) &
			" drawing line in layer" & to_string (layer) &
			" from " & to_string (device) & " terminal " & to_string (terminal) &
			" direction " & to_string (direction) &
			" along axis " & to_string (axis) &
			" grid notches " & to_string (notches),
			level => log_threshold);

		-- locate module and device
		module_cursor := locate_module (module_name);
		device_cursor := locate_device (module_cursor, device);
		-- CS call procedure device_not_found if
		-- device_cursor is no_element ?
		
		-- make sure the desired layer is available according to current layer stack:
		test_layer (module_cursor, layer);
		
		make_line (get_terminal_position (module_cursor, device_cursor, terminal));

		add_line (module_cursor, net_name, line, log_threshold + 1);
	end add_line;



	
	
	procedure add_line (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in pac_net_name.bounded_string; -- reset_n
		layer			: in et_pcb_stack.type_signal_layer;
		width			: in type_track_width;
		device			: in type_device_name;
		terminal		: in pac_terminal_name.bounded_string;
		end_point		: in type_vector_model;
		log_threshold	: in type_log_level) 
	is		
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		-- This is going to be the segment we will insert. In the follwing it
		-- will be tailored according to given terminal position and end point.
		-- Finally it will be added to the list of line segments (via procedure add_named_track)
		-- to the given net.
		line : type_conductor_line;

		use et_devices_electrical;		
		device_cursor : pac_devices_sch.cursor;

		
		procedure make_line (terminal_position : in type_terminal_position_fine) is begin

			-- Build the start point of the line:
			-- The start point of the line is always the x/y of the terminal.
			-- further-on set line width and layer.
			line := (
				A		=> to_point (terminal_position.place),
				width	=> width, -- as given by operator
				layer	=> layer, -- as given by operator
				B		=> end_point, -- as given by operator
				others	=> <>);

			check_terminal_face_vs_layer (module_cursor, terminal_position, layer);
			
		end make_line;

		
	begin
		log (text => "module " & to_string (module_name) &
			" " & to_string (net_name) &
			" drawing line in layer" & to_string (layer) &
			" from " & to_string (device) & " terminal " & to_string (terminal) &
			" to " & to_string (end_point),
			level => log_threshold);

		-- locate module and device
		module_cursor := locate_module (module_name);
		device_cursor := locate_device (module_cursor, device);
		-- CS call procedure device_not_found if
		-- device_cursor is no_element ?
		
		-- make sure the desired layer is available according to current layer stack:
		test_layer (module_cursor, layer);
		
		make_line (get_terminal_position (module_cursor, device_cursor, terminal));

		add_line (module_cursor, net_name, line, log_threshold + 1);
	end add_line;



	
	
	procedure add_line (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in pac_net_name.bounded_string; -- reset_n
		layer			: in et_pcb_stack.type_signal_layer;
		width			: in type_track_width;
		device			: in type_device_name;
		terminal		: in pac_terminal_name.bounded_string;
		axis			: in type_axis_2d;
		notches			: in type_grid_notches;
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		-- This is going to be the segment we will insert. In the follwing it
		-- will be tailored according to given terminal position, axis and grid notches.
		-- Finally it will be added to the list of line segments (via procedure add_named_track)
		-- to the given net.
		line : type_conductor_line;
		
		use et_devices_electrical;
		device_cursor : pac_devices_sch.cursor;
		
		procedure make_line (terminal_position : in type_terminal_position_fine) is begin

			-- Build the start point of the line:
			-- The start point of the line is always the x/y of the terminal.
			-- further-on set line width and layer.
			line := (
				A	=> to_point (terminal_position.place),
				width		=> width, -- as given by operator
				layer		=> layer, -- as given by operator
				others 		=> <>);

			check_terminal_face_vs_layer (module_cursor, terminal_position, layer);
			
			-- Build the end point of the line. It is the start point moved in direction:
			-- CS
			
		end make_line;

		
	begin
		log (text => "module " & to_string (module_name) &
			" " & to_string (net_name) &
			" drawing line in layer" & to_string (layer) &
			" from " & to_string (device) & " terminal " & to_string (terminal) &
			" along axis " & to_string (axis) &
			" grid notches " & to_string (notches),
			level => log_threshold);

		-- locate module and device
		module_cursor := locate_module (module_name);
		device_cursor := locate_device (module_cursor, device);
		-- CS call procedure device_not_found if
		-- device_cursor is no_element ?
		
		-- make sure the desired layer is available according to current layer stack:
		test_layer (module_cursor, layer);
		
		make_line (get_terminal_position (module_cursor, device_cursor, terminal));

		add_line (module_cursor, net_name, line, log_threshold + 1);
	end add_line;





	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		line			: in type_object_line_net;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level)
	is


		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is

				procedure query_line (l : in out type_conductor_line) is begin
					modify_status (l, operation);
				end query_line;

				use pac_conductor_lines;
			begin
				update_element (net.route.lines, line.line_cursor, query_line'access);
			end query_net;
			
		begin
			update_element (module.nets, line.net_cursor, query_net'access);
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " modifying status of "
			& to_string (line.line_cursor, true) -- incl. width
			& " in net " & to_string (line.net_cursor)
			& " / " & to_string (operation),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end modify_status;

	



	
	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		line			: in type_object_line_floating;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level)
	is


		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is

			procedure query_line (l : in out type_conductor_line) is begin
				modify_status (l, operation);
			end query_line;

			use pac_conductor_lines;
			
		begin
			update_element (module.board.conductors_floating.lines, line.line_cursor, query_line'access);
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " modifying status of floating "
			& to_string (line.line_cursor, true) -- incl. width
			& " / " & to_string (operation),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end modify_status;



	
	

	function get_lines (
		module_cursor	: in pac_generic_modules.cursor;
		layer			: in et_pcb_stack.type_signal_layer;
		catch_zone		: in type_catch_zone;
		log_threshold	: in type_log_level)
		return pac_object_lines.list
	is
		result : pac_object_lines.list;
		
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			use pac_nets;
			net_cursor : pac_nets.cursor := module.nets.first;

			
			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net 		: in type_net) 
			is
				use pac_conductor_lines;
				lc : pac_conductor_lines.cursor := net.route.lines.first;

				procedure query_line (line : in type_conductor_line) is begin
					if line.layer = layer then
						if in_catch_zone (
							zone	=> catch_zone,
							line	=> line,
							width	=> line.width)
						then
							result.append ((net_cursor, lc));
						end if;
					end if;
				end query_line;				
				
			begin
				query_element (lc, query_line'access);
			end query_net;

			
		begin
			while net_cursor /= pac_nets.no_element loop
				pac_nets.query_element (net_cursor, query_net'access);
				next (net_cursor);
			end loop;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " looking up line segments of nets"
			 & " in signal layer " & to_string (layer)
			 & to_string (catch_zone),
			 level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log (text => "found" & count_type'image (result.length),
			 level => log_threshold + 1);
		
		log_indentation_down;
		return result;
	end get_lines;


	

	
	
	function get_lines (
		module_cursor	: in pac_generic_modules.cursor;
		layer			: in et_pcb_stack.type_signal_layer;
		catch_zone		: in type_catch_zone;
		log_threshold	: in type_log_level)
		return pac_conductor_lines.list
	is
		result : pac_conductor_lines.list;


		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			net_name : pac_net_name.bounded_string;

			
			procedure query_line (c : in pac_conductor_lines.cursor) is
				use pac_conductor_lines;
				line : type_conductor_line renames element (c);
			begin
				if line.layer = layer then
					if in_catch_zone (
						zone	=> catch_zone,
						line	=> line,
						width	=> line.width)
					then
						result.append (line);
					end if;
				end if;
			end query_line;
		
			
		begin
			module.board.conductors_floating.lines.iterate (query_line'access);
		end query_module;


	begin
		log (text => "module " & to_string (module_cursor)
			& " looking up line segments of nets"
			 & " in signal layer " & to_string (layer)
			 & to_string (catch_zone),
			 level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log (text => "found" & get_length (result),
			level => log_threshold + 1);
		
		log_indentation_down;
		
		return result;
	end get_lines;


	
	

	
	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		line_cursor		: in pac_conductor_lines.cursor;
		operation		: in type_status_operation;
		freetracks		: in boolean;
		log_threshold	: in type_log_level)
	is
		use pac_conductor_lines;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			proceed : aliased boolean := true;


			procedure query_line (
				line : in out type_conductor_line)
			is begin
				modify_status (line, operation);
			end query_line;

			
			procedure process_freetracks is
				conductors : type_conductors_floating renames module.board.conductors_floating;
				l : pac_conductor_lines.cursor;
			begin
				l := conductors.lines.first;
				while l /= pac_conductor_lines.no_element and proceed loop
					if l = line_cursor then
						conductors.lines.update_element (l, query_line'access);
						proceed := false; -- abort iterator
					end if;
					
					next (l);
				end loop;
			end process_freetracks;
			

			
			procedure process_nets is
				
				procedure query_net (
					net_name	: in pac_net_name.bounded_string;
					net			: in out type_net)
				is
					l : pac_conductor_lines.cursor := net.route.lines.first;
				begin
					log (text => "net " & to_string (net_name), level => log_threshold + 1);
					
					while l /= pac_conductor_lines.no_element and proceed loop
						if l = line_cursor then
							net.route.lines.update_element (l, query_line'access);
							proceed := false; -- aborts the net iterator. see below.
						end if;

						next (l);
					end loop;
				end query_net;

				
				net_cursor : pac_nets.cursor := module.nets.first;			
			begin
				while net_cursor /= pac_nets.no_element and proceed loop
					module.nets.update_element (net_cursor, query_net'access);
					next (net_cursor);
				end loop;
			end process_nets;

			
		begin
			if freetracks then
				process_freetracks;
			else
				process_nets;
			end if;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " modifying status of "
			& to_string (element (line_cursor), true) -- log width
			& " / " & to_string (operation),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end modify_status;




	

	procedure propose_lines (
		module_cursor	: in pac_generic_modules.cursor;
		layer			: in et_pcb_stack.type_signal_layer;
		catch_zone		: in type_catch_zone;
		count			: in out natural;
		freetracks		: in boolean;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is

			use pac_conductor_lines;
			
			procedure query_line (
				line : in out type_conductor_line)
			is begin
				if line.layer = layer then
					if in_catch_zone (
						zone	=> catch_zone,
						line	=> line,
						width	=> line.width)
					then
						set_proposed (line);
						count := count + 1;
						log (text => to_string (line, true), level => log_threshold + 2);
					end if;
				end if;
			end query_line;
			


			procedure process_freetracks is
				line_cursor : pac_conductor_lines.cursor;
				segments : type_conductors_floating renames module.board.conductors_floating;
			begin
				line_cursor := segments.lines.first;
				while line_cursor /= pac_conductor_lines.no_element loop
					segments.lines.update_element (line_cursor, query_line'access);
					next (line_cursor);
				end loop;
			end process_freetracks;


			
			procedure process_nets is

				procedure query_net (
					net_name	: in pac_net_name.bounded_string;
					net			: in out type_net)
				is
					use et_nets;				
					line_cursor : pac_conductor_lines.cursor := net.route.lines.first;
				begin
					log (text => "net " & to_string (net_name), level => log_threshold + 1);
					log_indentation_up;
					
					while line_cursor /= pac_conductor_lines.no_element loop
						net.route.lines.update_element (line_cursor, query_line'access);
						next (line_cursor);
					end loop;

					log_indentation_down;
				end query_net;


				net_cursor : pac_nets.cursor := module.nets.first;
			begin
				while net_cursor /= pac_nets.no_element loop
					module.nets.update_element (net_cursor, query_net'access);
					next (net_cursor);
				end loop;
			end process_nets;
			
			
		begin
			if freetracks then
				process_freetracks;
			else
				process_nets;
			end if;
		end query_module;
		
		
	begin
		log (text => "proposing lines in signal layer " & to_string (layer)
			 & to_string (catch_zone),
			 level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end propose_lines;


	


	
	procedure reset_proposed_lines (
		module_cursor	: in pac_generic_modules.cursor;
		freetracks		: in boolean;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use pac_conductor_lines;

			
			procedure query_line (
				line : in out type_conductor_line)
			is 
				use et_object_status;
			begin
				reset_status (line);
			end query_line;


			
			procedure process_nets is
				
				procedure query_net (
					net_name	: in pac_net_name.bounded_string;
					net			: in out type_net)
				is
					use et_nets;					
					line_cursor : pac_conductor_lines.cursor := net.route.lines.first;
				begin
					while line_cursor /= pac_conductor_lines.no_element loop
						net.route.lines.update_element (line_cursor, query_line'access);
						next (line_cursor);
					end loop;
				end query_net;				

				net_cursor : pac_nets.cursor := module.nets.first;
			begin			
				while net_cursor /= pac_nets.no_element loop
					module.nets.update_element (net_cursor, query_net'access);
					next (net_cursor);
				end loop;
			end process_nets;



			procedure process_freetracks is
				line_cursor : pac_conductor_lines.cursor := module.board.conductors_floating.lines.first;
			begin
				while line_cursor /= pac_conductor_lines.no_element loop
					module.board.conductors_floating.lines.update_element (line_cursor, query_line'access);
					next (line_cursor);
				end loop;
			end process_freetracks;

			
			
		begin
			if freetracks then
				process_freetracks;
			else
				process_nets;
			end if;
		end query_module;
	

		
	begin
		log (text => "resetting proposed lines",
			 level => log_threshold);

		log_indentation_up;

		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end reset_proposed_lines;
	


	
	
	function get_first_line_net (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;
		log_threshold	: in type_log_level)
		return type_object_line_net
	is
		result : type_object_line_net;

		use pac_conductor_lines;
		

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			proceed : aliased boolean := true;


			procedure process_nets is

				procedure query_net (net_cursor : in pac_nets.cursor) is

					procedure query_lines (
						net_name	: in pac_net_name.bounded_string;
						net 		: in type_net)
					is 

						procedure query_line (l : in pac_conductor_lines.cursor) is begin
							case flag is
								when PROPOSED =>
									if is_proposed (l) then
										result.net_cursor := net_cursor;
										result.line_cursor := l;
										proceed := false;  -- no further probing required
										--log (text => to_string (line, true), level => log_threshold + 2);
									end if;

								when SELECTED =>
									if is_selected (l) then
										result.net_cursor := net_cursor;
										result.line_cursor := l;
										proceed := false;  -- no further probing required
										--log (text => to_string (line, true), level => log_threshold + 2);
									end if;

								when others =>
									null; -- CS
							end case;
						end query_line;


					begin
						iterate (net.route.lines, query_line'access, proceed'access);
					end query_lines;
					
				begin
					log (text => "net " & to_string (key (net_cursor)), level => log_threshold + 1);
					log_indentation_up;
					query_element (net_cursor, query_lines'access);
					log_indentation_down;
				end query_net;
				

			begin
				iterate (module.nets, query_net'access, proceed'access);
			end process_nets;

			
		begin
			process_nets;
		end query_module;


	begin		
		log (text => "module " & to_string (module_cursor)
			& " looking up the first line / " & to_string (flag),
			level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_module'access);

		-- If a line has been found, then log it:
		if has_element (result.line_cursor) then
			log (text => "found: " & to_string (element (result.line_cursor), true),
				 level => log_threshold + 2);
		end if;

		log_indentation_down;
		
		return result;
	end get_first_line_net;






	function get_first_line_floating (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;
		log_threshold	: in type_log_level)
		return type_object_line_floating
	is
		result : type_object_line_floating;

		use pac_conductor_lines;
		

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			proceed : aliased boolean := true;


			conductors : type_conductors_floating renames module.board.conductors_floating;

			procedure query_line (l : in pac_conductor_lines.cursor) is
				line : type_conductor_line renames element (l);
			begin
				case flag is
					when PROPOSED =>
						if is_proposed (line) then
							result.line_cursor := l;
							proceed := false;  -- no further probing required
						end if;

					when SELECTED =>
						if is_selected (line) then
							result.line_cursor := l;
							proceed := false;  -- no further probing required
						end if;

					when others =>
						null; -- CS
				end case;
			end query_line;
				
			
		begin
			iterate (conductors.lines, query_line'access, proceed'access);
		end query_module;


	begin		
		log (text => "module " & to_string (module_cursor)
			& " looking up the first floating line / " & to_string (flag),
			level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_module'access);

		-- If a line has been found, then log it:
		if has_element (result.line_cursor) then
			log (text => "found: " & to_string (element (result.line_cursor), true),
				 level => log_threshold + 2);
		end if;

		log_indentation_down;
		
		return result;
	end get_first_line_floating;


	
	

	
	procedure next_proposed_line (
		module_cursor	: in pac_generic_modules.cursor;
		line			: in out type_object_line_net;
		freetracks		: in boolean;
		-- last_item		: in out boolean;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module)
		is
			use pac_conductor_lines;

			
			procedure process_freetracks is
				proceed : boolean := true;

				l : pac_conductor_lines.cursor;
				conductors : type_conductors_floating renames module.board.conductors_floating;

			begin
				-- Start with the line after the given line:
				l := next (line.line_cursor);
				while l /= pac_conductor_lines.no_element and proceed loop
					if is_proposed (element (l)) then
						proceed := false;
						line.line_cursor := l;
					end if;
					next (l);
				end loop;

				-- If no proposed line has been found so far, then
				-- start the search with the first line and traverse
				-- until the line before the given line:
				if proceed then
					l := conductors.lines.first;

					while l /= line.line_cursor and proceed loop
						if is_proposed (element (l)) then
							proceed := false;
							line.line_cursor := l;
						end if;
						next (l);
					end loop;
				end if;					
			end process_freetracks;


			
			
			procedure process_nets is
				use et_nets;
				
				-- The serach for the next proposed line starts at the
				-- given line. This flag is used to initiate the search:
				init : boolean := true;

				-- This flag indicates that a proposed line has been found
				-- while probing the lines of a net. If a proposed line
				-- has been found, then it is cleared so that the search
				-- procedure is cancelled:
				proceed : boolean := true;

				-- A temporarily cursor that points to the net being
				-- searched in. The search starts with the given net:
				nc : pac_nets.cursor := line.net_cursor;
				
				
				procedure query_net (
					net_name	: in pac_net_name.bounded_string;
					net			: in type_net)
				is
					-- A temporarily cursor that points to the
					-- line segment being probed:
					lc : pac_conductor_lines.cursor;
				begin
					-- The first call of this procedure addresses the GIVEN
					-- net. Inside this net we start probing the lines
					-- at the GIVEN line.
					if init then
						lc := line.line_cursor; -- go to given line
						init := false;

						-- If the given line is not the last one, then
						-- advance to the next line after the given line.
						-- If the given line is the last, then do nothing:
						if lc /= net.route.lines.last then
							next (lc);

							-- Iterate the lines starting at the GIVEN line
							-- until the last line. Cancel the iteration
							-- if the candidate line is proposed and set
							-- the given line accordingly:
							while lc /= pac_conductor_lines.no_element loop
								if is_proposed (element (lc)) then
									line.line_cursor := lc;
									line.net_cursor := nc;
									proceed := false; -- abort net iterator (see below)
									exit; -- no further probing required:
								else
									next (lc);
								end if;
							end loop;
						end if;
						
					else
					-- For all further calls of this procedure:
					-- Iterate the lines of the net starting at the first line.
					-- Cancel the iteration if the candidate line is proposed and set
					-- the given line accordingly:
						lc := net.route.lines.first;

						while lc /= pac_conductor_lines.no_element loop
							if is_proposed (element (lc)) then
								line.line_cursor := lc;
								line.net_cursor := nc;
								proceed := false; -- abort net iterator (see below)
								exit; -- no further probing required:
							else
								next (lc);
							end if;
						end loop;					
					end if;				
				end query_net;

				
			begin
				-- Query the nets one by one until the last net.
				-- Start at the GIVEN net. The iteration is cancelled once 
				-- the proceed-flag is cleared by procedure query_net:
				while nc /= pac_nets.no_element and proceed loop
					query_element (nc, query_net'access);
					next (nc);
				end loop;

				-- If proceed is still true, means no proposed line has been found
				-- so far, then restart the search at the FIRST net.
				if proceed then
					nc := module.nets.first;

					-- Query the nets one by one until the last net.
					while nc /= pac_nets.no_element and proceed loop
						query_element (nc, query_net'access);
						next (nc);
					end loop;

					-- If proceed ist still true, then set all
					-- selectors of line to no_element:
					if proceed then
						line := (others => <>);
					end if;
				end if;				
			end process_nets;
				
			
		begin
			if freetracks then
				process_freetracks;
			else
				process_nets;
			end if;			
		end query_module;
		
		
	begin
		log (text => "module " & to_string (module_cursor)
			& " advancing to next proposed line",
			level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end next_proposed_line;

	


	
	
	procedure move_line_net (
		module_cursor	: in pac_generic_modules.cursor;
		line			: in type_object_line_net;
		point_of_attack	: in type_vector_model;
		destination		: in type_vector_model;
		log_threshold	: in type_log_level)
	is
		use pac_conductor_lines;
		use et_nets;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is

			procedure update_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is
				procedure move (line : in out type_conductor_line) is begin
					attack (line, point_of_attack, destination);
				end;

			begin
				log (text => "net " & to_string (net_name), level => log_threshold + 1);
				net.route.lines.update_element (line.line_cursor, move'access);
			end update_net;

			
		begin
			module.nets.update_element (line.net_cursor, update_net'access);			
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " moving " & to_string (line.line_cursor, true)  -- log incl. width
			& " point of attack " & to_string (point_of_attack)
			& " to" & to_string (destination),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (						
			position	=> module_cursor,
			process		=> query_module'access);
		
		log_indentation_down;

		update_ratsnest (module_cursor, log_threshold + 1);
	end move_line_net;





	

	procedure move_line_floating (
		module_cursor	: in pac_generic_modules.cursor;
		line			: in type_object_line_floating;
		point_of_attack	: in type_vector_model;
		destination		: in type_vector_model;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is

			use pac_conductor_lines;

			procedure move (line : in out type_conductor_line) is begin
				attack (line, point_of_attack, destination);
				log (text => (to_string (line, true)), level => log_threshold + 1);
			end;
	
		begin
			module.board.conductors_floating.lines.update_element (line.line_cursor, move'access);
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " moving floating " & to_string (line.line_cursor, true)  -- log incl. width
			& " point of attack " & to_string (point_of_attack)
			& " to" & to_string (destination),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (						
			position	=> module_cursor,
			process		=> query_module'access);
		
		log_indentation_down;
	end move_line_floating;



	
	

	procedure delete_line_net (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in pac_net_name.bounded_string; -- reset_n
		line			: in type_conductor_line;
		log_threshold	: in type_log_level)
	is


		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			-- Locate the given net in the given module::
			net_cursor : pac_nets.cursor := find (module.nets, net_name);

			use et_nets;
			
			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net) 
			is
				-- Locate the given segment in the given net:
				use pac_conductor_lines;
				line_cursor : pac_conductor_lines.cursor := net.route.lines.find (line);
			begin
				if line_cursor /= pac_conductor_lines.no_element then
					delete (net.route.lines, line_cursor);
				else
					null; -- CS message "segment not found" ?
				end if;
			end query_net;
			

		begin			
			if net_exists (net_cursor) then

				pac_nets.update_element (
					container	=> module.nets,
					position	=> net_cursor,
					process		=> query_net'access);

			else
				net_not_found (net_name);
			end if;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor) &
			" net " & to_string (net_name) &
			" deleting segment" & to_string (line, true), -- log linewidth
			level => log_threshold);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);

		update_ratsnest (module_cursor, log_threshold + 1);
	end delete_line_net;


	



	procedure delete_line_floating (
		module_cursor	: in pac_generic_modules.cursor;
		line			: in type_conductor_line;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			conductors : type_conductors_floating renames module.board.conductors_floating;
			use pac_conductor_lines;
			l : pac_conductor_lines.cursor;
		begin
			-- Locate the line:
			l := find (conductors.lines, line);

			-- If the line exists, then delete it:
			if l /= pac_conductor_lines.no_element then
				delete (conductors.lines, l);
			end if;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor) &
			" deleting segment" & to_string (line, true), -- log linewidth
			level => log_threshold);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);
		
	end delete_line_floating;



	
	
-- ARCS:
	
	
	procedure add_arc (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in pac_net_name.bounded_string; -- reset_n
		arc				: in type_conductor_arc;
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		use pac_conductor_arcs;

		
		procedure add_freetrack (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is begin
			append (
				container	=> module.board.conductors_floating.arcs,
				new_item	=> arc);
		end;

		
		procedure add_named_track (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			-- A track belonging to a net requires the net to be located in the given module:
			net_cursor : pac_nets.cursor := find (module.nets, net_name);

			use et_nets;
			
			-- Appends the track to the net.
			procedure add (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net) 
			is begin
				append (
					container	=> net.route.arcs,
					new_item	=> arc);
			end add;

		begin
			if net_exists (net_cursor) then

				pac_nets.update_element (
					container	=> module.nets,
					position	=> net_cursor,
					process		=> add'access);
				
			else
				net_not_found (net_name);
			end if;
		end add_named_track;

		
	begin
		log (text => "module " & to_string (module_name) &
			 freetrack (net_name) &
			" drawing arc" &
			" in layer" & to_string (arc.layer) &
			to_string (arc),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);

		-- make sure the desired layer is available according to current layer stack:
		test_layer (module_cursor, arc.layer);
		
		if is_freetrack (net_name) then
			
			update_element (
				container	=> generic_modules,
				position	=> module_cursor,
				process		=> add_freetrack'access);

		else
			update_element (
				container	=> generic_modules,
				position	=> module_cursor,
				process		=> add_named_track'access);

			update_ratsnest (module_cursor, log_threshold + 1);
		end if;
	end add_arc;

	



	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		arc				: in type_object_arc_net;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level)
	is


		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is

				procedure query_arc (l : in out type_conductor_arc) is begin
					modify_status (l, operation);
				end query_arc;

				use pac_conductor_arcs;
			begin
				update_element (net.route.arcs, arc.arc_cursor, query_arc'access);
			end query_net;
			
		begin
			update_element (module.nets, arc.net_cursor, query_net'access);
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " modifying status of "
			& to_string (arc.arc_cursor, true) -- incl. width
			& " in net " & to_string (arc.net_cursor)
			& " / " & to_string (operation),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end modify_status;

	



	
	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		arc				: in type_object_arc_floating;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level)
	is


		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is

			procedure query_arc (l : in out type_conductor_arc) is begin
				modify_status (l, operation);
			end query_arc;

			use pac_conductor_arcs;
			
		begin
			update_element (module.board.conductors_floating.arcs, arc.arc_cursor, query_arc'access);
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " modifying status of floating "
			& to_string (arc.arc_cursor, true) -- incl. width
			& " / " & to_string (operation),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end modify_status;





	procedure propose_arcs (
		module_cursor	: in pac_generic_modules.cursor;
		layer			: in et_pcb_stack.type_signal_layer;
		catch_zone		: in type_catch_zone;
		count			: in out natural;
		freetracks		: in boolean;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is

			use pac_conductor_arcs;
			
			procedure query_arc (
				arc : in out type_conductor_arc)
			is begin
				if arc.layer = layer then
					if in_catch_zone (
						zone	=> catch_zone,
						arc	=> arc,
						width	=> arc.width)
					then
						set_proposed (arc);
						count := count + 1;
						log (text => to_string (arc, true), level => log_threshold + 2);
					end if;
				end if;
			end query_arc;
			


			procedure process_freetracks is
				arc_cursor : pac_conductor_arcs.cursor;
				segments : type_conductors_floating renames module.board.conductors_floating;
			begin
				arc_cursor := segments.arcs.first;
				while arc_cursor /= pac_conductor_arcs.no_element loop
					segments.arcs.update_element (arc_cursor, query_arc'access);
					next (arc_cursor);
				end loop;
			end process_freetracks;


			
			procedure process_nets is

				procedure query_net (
					net_name	: in pac_net_name.bounded_string;
					net			: in out type_net)
				is
					use et_nets;				
					arc_cursor : pac_conductor_arcs.cursor := net.route.arcs.first;
				begin
					log (text => "net " & to_string (net_name), level => log_threshold + 1);
					log_indentation_up;
					
					while arc_cursor /= pac_conductor_arcs.no_element loop
						net.route.arcs.update_element (arc_cursor, query_arc'access);
						next (arc_cursor);
					end loop;

					log_indentation_down;
				end query_net;


				net_cursor : pac_nets.cursor := module.nets.first;
			begin
				while net_cursor /= pac_nets.no_element loop
					module.nets.update_element (net_cursor, query_net'access);
					next (net_cursor);
				end loop;
			end process_nets;
			
			
		begin
			if freetracks then
				process_freetracks;
			else
				process_nets;
			end if;
		end query_module;
		
		
	begin
		log (text => "proposing arcs in signal layer " & to_string (layer)
			 & to_string (catch_zone),
			 level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end propose_arcs;



	


	procedure reset_proposed_arcs (
		module_cursor	: in pac_generic_modules.cursor;
		freetracks		: in boolean;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use pac_conductor_arcs;

			
			procedure query_arc (
				arc : in out type_conductor_arc)
			is 
				use et_object_status;
			begin
				reset_status (arc);
			end query_arc;


			
			procedure process_nets is
				
				procedure query_net (
					net_name	: in pac_net_name.bounded_string;
					net			: in out type_net)
				is
					use et_nets;					
					arc_cursor : pac_conductor_arcs.cursor := net.route.arcs.first;
				begin
					while arc_cursor /= pac_conductor_arcs.no_element loop
						net.route.arcs.update_element (arc_cursor, query_arc'access);
						next (arc_cursor);
					end loop;
				end query_net;				

				net_cursor : pac_nets.cursor := module.nets.first;
			begin			
				while net_cursor /= pac_nets.no_element loop
					module.nets.update_element (net_cursor, query_net'access);
					next (net_cursor);
				end loop;
			end process_nets;



			procedure process_freetracks is
				arc_cursor : pac_conductor_arcs.cursor := module.board.conductors_floating.arcs.first;
			begin
				while arc_cursor /= pac_conductor_arcs.no_element loop
					module.board.conductors_floating.arcs.update_element (arc_cursor, query_arc'access);
					next (arc_cursor);
				end loop;
			end process_freetracks;

			
			
		begin
			if freetracks then
				process_freetracks;
			else
				process_nets;
			end if;
		end query_module;
	

		
	begin
		log (text => "resetting proposed arcs",
			 level => log_threshold);

		log_indentation_up;

		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end reset_proposed_arcs;
	

	
	


	function get_first_arc_net (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;
		log_threshold	: in type_log_level)
		return type_object_arc_net
	is
		result : type_object_arc_net;

		use pac_conductor_arcs;
		

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			proceed : aliased boolean := true;


			procedure process_nets is

				procedure query_net (net_cursor : in pac_nets.cursor) is

					procedure query_arcs (
						net_name	: in pac_net_name.bounded_string;
						net 		: in type_net)
					is 

						procedure query_arc (l : in pac_conductor_arcs.cursor) is begin
							case flag is
								when PROPOSED =>
									if is_proposed (l) then
										result.net_cursor := net_cursor;
										result.arc_cursor := l;
										proceed := false;  -- no further probing required
										--log (text => to_string (arc, true), level => log_threshold + 2);
									end if;

								when SELECTED =>
									if is_selected (l) then
										result.net_cursor := net_cursor;
										result.arc_cursor := l;
										proceed := false;  -- no further probing required
										--log (text => to_string (arc, true), level => log_threshold + 2);
									end if;

								when others =>
									null; -- CS
							end case;
						end query_arc;


					begin
						iterate (net.route.arcs, query_arc'access, proceed'access);
					end query_arcs;
					
				begin
					log (text => "net " & to_string (key (net_cursor)), level => log_threshold + 1);
					log_indentation_up;
					query_element (net_cursor, query_arcs'access);
					log_indentation_down;
				end query_net;
				

			begin
				iterate (module.nets, query_net'access, proceed'access);
			end process_nets;

			
		begin
			process_nets;
		end query_module;


	begin		
		log (text => "module " & to_string (module_cursor)
			& " looking up the first arc / " & to_string (flag),
			level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_module'access);

		-- If a arc has been found, then log it:
		if has_element (result.arc_cursor) then
			log (text => "found: " & to_string (element (result.arc_cursor), true),
				 level => log_threshold + 2);
		end if;

		log_indentation_down;
		
		return result;
	end get_first_arc_net;






	function get_first_arc_floating (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;
		log_threshold	: in type_log_level)
		return type_object_arc_floating
	is
		result : type_object_arc_floating;

		use pac_conductor_arcs;
		

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			proceed : aliased boolean := true;


			conductors : type_conductors_floating renames module.board.conductors_floating;

			procedure query_arc (l : in pac_conductor_arcs.cursor) is
				arc : type_conductor_arc renames element (l);
			begin
				case flag is
					when PROPOSED =>
						if is_proposed (arc) then
							result.arc_cursor := l;
							proceed := false;  -- no further probing required
						end if;

					when SELECTED =>
						if is_selected (arc) then
							result.arc_cursor := l;
							proceed := false;  -- no further probing required
						end if;

					when others =>
						null; -- CS
				end case;
			end query_arc;
				
			
		begin
			iterate (conductors.arcs, query_arc'access, proceed'access);
		end query_module;


	begin		
		log (text => "module " & to_string (module_cursor)
			& " looking up the first floating arc / " & to_string (flag),
			level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_module'access);

		-- If a arc has been found, then log it:
		if has_element (result.arc_cursor) then
			log (text => "found: " & to_string (element (result.arc_cursor), true),
				 level => log_threshold + 2);
		end if;

		log_indentation_down;
		
		return result;
	end get_first_arc_floating;



	

	
	procedure move_arc_net (
		module_cursor	: in pac_generic_modules.cursor;
		arc				: in type_object_arc_net;
		point_of_attack	: in type_vector_model;
		destination		: in type_vector_model;
		log_threshold	: in type_log_level)
	is
		use pac_conductor_arcs;
		use et_nets;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is

			procedure update_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is
				procedure move (arc : in out type_conductor_arc) is begin
					attack (arc, point_of_attack, destination);
				end;

			begin
				log (text => "net " & to_string (net_name), level => log_threshold + 1);
				net.route.arcs.update_element (arc.arc_cursor, move'access);
			end update_net;

			
		begin
			module.nets.update_element (arc.net_cursor, update_net'access);			
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " moving " & to_string (arc.arc_cursor, true)  -- log incl. width
			& " point of attack " & to_string (point_of_attack)
			& " to" & to_string (destination),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (						
			position	=> module_cursor,
			process		=> query_module'access);
		
		log_indentation_down;

		update_ratsnest (module_cursor, log_threshold + 1);
	end move_arc_net;





	

	procedure move_arc_floating (
		module_cursor	: in pac_generic_modules.cursor;
		arc				: in type_object_arc_floating;
		point_of_attack	: in type_vector_model;
		destination		: in type_vector_model;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is

			use pac_conductor_arcs;

			procedure move (arc : in out type_conductor_arc) is begin
				attack (arc, point_of_attack, destination);
				log (text => (to_string (arc, true)), level => log_threshold + 1);
			end;
	
		begin
			module.board.conductors_floating.arcs.update_element (arc.arc_cursor, move'access);
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " moving floating " & to_string (arc.arc_cursor, true)  -- log incl. width
			& " point of attack " & to_string (point_of_attack)
			& " to" & to_string (destination),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (						
			position	=> module_cursor,
			process		=> query_module'access);
		
		log_indentation_down;
	end move_arc_floating;



	


	procedure delete_arc_net (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in pac_net_name.bounded_string; -- reset_n
		arc				: in type_conductor_arc;
		log_threshold	: in type_log_level)
	is


		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			-- Locate the given net in the given module::
			net_cursor : pac_nets.cursor := find (module.nets, net_name);

			use et_nets;
			
			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net) 
			is
				-- Locate the given segment in the given net:
				use pac_conductor_arcs;
				arc_cursor : pac_conductor_arcs.cursor := net.route.arcs.find (arc);
			begin
				if arc_cursor /= pac_conductor_arcs.no_element then
					delete (net.route.arcs, arc_cursor);
				else
					null; -- CS message "segment not found" ?
				end if;
			end query_net;
			

		begin			
			if net_exists (net_cursor) then

				pac_nets.update_element (
					container	=> module.nets,
					position	=> net_cursor,
					process		=> query_net'access);

			else
				net_not_found (net_name);
			end if;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor) &
			" net " & to_string (net_name) &
			" deleting segment" & to_string (arc, true), -- log linewidth
			level => log_threshold);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);

		update_ratsnest (module_cursor, log_threshold + 1);
	end delete_arc_net;


	



	procedure delete_arc_floating (
		module_cursor	: in pac_generic_modules.cursor;
		arc				: in type_conductor_arc;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			conductors : type_conductors_floating renames module.board.conductors_floating;
			use pac_conductor_arcs;
			l : pac_conductor_arcs.cursor;
		begin
			-- Locate the arc:
			l := find (conductors.arcs, arc);

			-- If the arc exists, then delete it:
			if l /= pac_conductor_arcs.no_element then
				delete (conductors.arcs, l);
			end if;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor) &
			" deleting segment" & to_string (arc, true), -- log linewidth
			level => log_threshold);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);
		
	end delete_arc_floating;




	

	
	
	procedure delete_track (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in pac_net_name.bounded_string; -- reset_n
		layer			: in et_pcb_stack.type_signal_layer;
		catch_zone		: in type_catch_zone;
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		use pac_conductor_lines;
		use pac_conductor_arcs;

		deleted : boolean := false; -- goes true if at least one segment has been ripup

		
		procedure ripup_freetrack (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			line_cursor : pac_conductor_lines.cursor := module.board.conductors_floating.lines.first;
			arc_cursor  : pac_conductor_arcs.cursor := module.board.conductors_floating.arcs.first;
		begin
			-- first probe the lines. If a matching line found, delete it 
			-- and abort iteration.
			while line_cursor /= pac_conductor_lines.no_element loop

				if on_segment (get_center (catch_zone), layer, line_cursor) then
					delete (module.board.conductors_floating.lines, line_cursor);
					deleted := true;
					exit;
				end if;

				next (line_cursor);
			end loop;

			-- probe arcs if no line found.
			-- If a matching arc found, delete it and abort iteration.
			if not deleted then
				while arc_cursor /= pac_conductor_arcs.no_element loop

					if on_segment (get_center (catch_zone), layer, arc_cursor) then
						delete (module.board.conductors_floating.arcs, arc_cursor);
						deleted := true;
						exit;
					end if;
					
					next (arc_cursor);
				end loop;
			end if;

			-- if no line and no arc found, issue warning:
			if not deleted then
				no_net_segment_found (layer, catch_zone);
			end if;
			
		end ripup_freetrack;

		
		
		procedure ripup_named_track (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			-- Locate the given net in the given module:
			net_cursor : pac_nets.cursor := find (module.nets, net_name);

			use et_nets;
			
			procedure ripup (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net) 
			is
				line_cursor : pac_conductor_lines.cursor := net.route.lines.first;
				arc_cursor  : pac_conductor_arcs.cursor := net.route.arcs.first;
			begin
				-- first probe the lines. If a matching line found, delete it 
				-- and abort iteration.
				while line_cursor /= pac_conductor_lines.no_element loop

					if on_segment (get_center (catch_zone), layer, line_cursor) then
						delete (net.route.lines, line_cursor);
						deleted := true;
						exit;
					end if;

					next (line_cursor);
				end loop;

				-- probe arcs if no line found.
				-- If a matching arc found, delete it and abort iteration.
				if not deleted then
					while arc_cursor /= pac_conductor_arcs.no_element loop

						if on_segment (get_center (catch_zone), layer, arc_cursor) then
							delete (net.route.arcs, arc_cursor);
							deleted := true;
							exit;
						end if;
						
						next (arc_cursor);
					end loop;
				end if;

				-- if no line and no arc found, issue warning:
				if not deleted then
					no_net_segment_found (layer, catch_zone);
				end if;

			end ripup;

			
		begin -- ripup_named_track
			if net_exists (net_cursor) then

				pac_nets.update_element (
					container	=> module.nets,
					position	=> net_cursor,
					process		=> ripup'access);

			else
				net_not_found (net_name);
			end if;

		end ripup_named_track;

		
	begin -- delete_track
		log (text => "module " & to_string (module_name) &
			freetrack (net_name) &
			" deleting segment" &
			" in layer " & to_string (layer) &
			" in " & to_string (catch_zone),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);

		-- make sure the desired layer is available according to current layer stack:
		test_layer (module_cursor, layer);
		
		if is_freetrack (net_name) then
			
			update_element (
				container	=> generic_modules,
				position	=> module_cursor,
				process		=> ripup_freetrack'access);

		else
			update_element (
				container	=> generic_modules,
				position	=> module_cursor,
				process		=> ripup_named_track'access);

			update_ratsnest (module_cursor, log_threshold + 1);
		end if;		
	end delete_track;

	

	



	

	procedure ripup_net (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in pac_net_name.bounded_string; -- reset_n
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			-- Locate the given net in the given module::
			net_cursor : pac_nets.cursor := find (module.nets, net_name);

			use et_nets;
			
			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net) 
			is
				-- Locate the given segment in the given net:
				use pac_conductor_lines;
			begin
				net.route.lines.clear;
				net.route.arcs.clear;
				-- CS net.route.circles.clear;
			end query_net;
			

		begin			
			if net_exists (net_cursor) then

				pac_nets.update_element (
					container	=> module.nets,
					position	=> net_cursor,
					process		=> query_net'access);

			else
				net_not_found (net_name);
			end if;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor) &
			" net " & to_string (net_name) &
			" deleting all segments",
			level => log_threshold);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);

		update_ratsnest (module_cursor, log_threshold + 1);
	end ripup_net;




	

	procedure add_zone (
		module_cursor	: in pac_generic_modules.cursor;
		zone			: in type_zone'class;
		log_threshold	: in type_log_level;
		net_name		: in pac_net_name.bounded_string := et_net_names.no_name)
	is
		use ada.tags;
		use et_nets;
		
		
		procedure floating_solid (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use pac_floating_solid;

			p : type_floating_solid := 
				type_floating_solid (zone);
			
		begin
			log (text => to_string (p, p.properties),
				level => log_threshold + 1);

			module.board.conductors_floating.zones.solid.append (p);
		end floating_solid;

		
		procedure floating_hatched (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use pac_floating_hatched;

			p : type_floating_hatched := 
				type_floating_hatched (zone);
			
		begin
			log (text => to_string (p, p.properties),
				level => log_threshold + 1);

			module.board.conductors_floating.zones.hatched.append (p);
		end floating_hatched;

		-- Polygons which are connected with a net are part of a route.
		-- They must be added to the targeted net. So we need a cursor
		-- to the targeted net:
		net_cursor : pac_nets.cursor;

		
		procedure locate_targeted_net is begin
			net_cursor := locate_net (module_cursor, net_name);

			if net_cursor = pac_nets.no_element then
				raise semantic_error_1 with
					"ERROR: Net " & enclose_in_quotes (to_string (net_name)) 
					& " does not exist !";
			end if;
		end locate_targeted_net;

		
		procedure route_solid (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use pac_route_solid;

			p : type_route_solid := 
				type_route_solid (zone);


			procedure add_polygon (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is begin
				net.route.zones.solid.append (p);
			end add_polygon;
			
		begin --route_solid
			log (text => to_string (p, p.properties, net_name),
				level => log_threshold + 1);

			update_element (
				container	=> module.nets,
				position	=> net_cursor,
				process		=> add_polygon'access);
			
		end route_solid;

		
		procedure route_hatched (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use pac_route_hatched;

			p : type_route_hatched := 
				type_route_hatched (zone);
			
			procedure add_polygon (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is begin
				net.route.zones.hatched.append (p);
			end add_polygon;

		begin -- route_hatched
			log (text => to_string (p, p.properties, net_name),
				level => log_threshold + 1);

			update_element (
				container	=> module.nets,
				position	=> net_cursor,
				process		=> add_polygon'access);

		end route_hatched;

		
	begin -- add_zone
		log (text => "module " & to_string (module_cursor)
			& " placing fill zone in conductor layer ...",
			level => log_threshold);

		log_indentation_up;
		
		-- floating:
		if zone'tag = type_floating_solid'tag then

			update_element (
				container	=> generic_modules,
				position	=> module_cursor,
				process		=> floating_solid'access);

		elsif zone'tag = type_floating_hatched'tag then

			update_element (
				container	=> generic_modules,
				position	=> module_cursor,
				process		=> floating_hatched'access);


		-- route:
		elsif zone'tag = type_route_solid'tag then

			locate_targeted_net;
						
			update_element (
				container	=> generic_modules,
				position	=> module_cursor,
				process		=> route_solid'access);

		elsif zone'tag = type_route_hatched'tag then

			locate_targeted_net;

			update_element (
				container	=> generic_modules,
				position	=> module_cursor,
				process		=> route_hatched'access);
			
		else
			null; -- CS ?
		end if;
		
		log_indentation_down;
	end add_zone;




	

	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		segment			: in type_object_segment_net;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level)
	is
		use pac_contours;
		use pac_segments;
		use pac_route_solid;
		use pac_route_hatched;
		
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			
			procedure query_segment (
				segment	: in out type_segment)
			is begin
				modify_status (segment, operation);
			end query_segment;

			
			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is 
				
				procedure query_zone_solid (zone : in out type_route_solid) is begin
					if is_circular (zone) then
						null; -- CS
					else
						-- Locate the given segment in the
						-- candidate zone:
						update_element (
							container	=> zone.contour.segments,
							position	=> segment.segment,
							process		=> query_segment'access);
					end if;
				end query_zone_solid;

				
				procedure query_zone_hatched (zone : in out type_route_hatched) is begin
					if is_circular (zone) then
						null; -- CS
					else
						-- Locate the given segment in the
						-- candidate zone:
						update_element (
							container	=> zone.contour.segments,
							position	=> segment.segment,
							process		=> query_segment'access);
					end if;
				end query_zone_hatched;

				
			begin
				-- Locate the zone:
				case segment.fill_style is
					when SOLID =>
						update_element (
							container	=> net.route.zones.solid,
							position	=> segment.zone_solid,
							process		=> query_zone_solid'access);

					when HATCHED =>
						update_element (
							container	=> net.route.zones.hatched,
							position	=> segment.zone_hatched,
							process		=> query_zone_hatched'access);
				end case;
			end query_net;
	
			
		begin
			-- Search the given segment according to its net:
			update_element (
				container	=> module.nets, 
				position	=> segment.net, 
				process		=> query_net'access);

		end query_module;
		
		
	begin
		log (text => "module " & to_string (module_cursor)
			& " modifying status of "
			& to_string (segment.segment)
			& " net " & to_string (segment.net)
			& " / " & to_string (operation),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end modify_status;





	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		segment			: in type_object_segment_floating;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level)
	is
		use pac_contours;
		use pac_segments;
		use pac_floating_solid;
		use pac_floating_hatched;
		
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			
			procedure query_segment (
				segment	: in out type_segment)
			is begin
				modify_status (segment, operation);
			end query_segment;

			
				
			procedure query_zone_solid (zone : in out type_floating_solid) is begin
				if is_circular (zone) then
					null; -- CS
				else
					-- Locate the given segment in the
					-- candidate zone:
					update_element (
						container	=> zone.contour.segments,
						position	=> segment.segment,
						process		=> query_segment'access);
				end if;
			end query_zone_solid;

			
			procedure query_zone_hatched (zone : in out type_floating_hatched) is begin
				if is_circular (zone) then
					null; -- CS
				else
					-- Locate the given segment in the
					-- candidate zone:
					update_element (
						container	=> zone.contour.segments,
						position	=> segment.segment,
						process		=> query_segment'access);
				end if;
			end query_zone_hatched;

	
			
		begin
			-- Locate the zone:
			case segment.fill_style is
				when SOLID =>
					update_element (
						container	=> module.board.conductors_floating.zones.solid,
						position	=> segment.zone_solid,
						process		=> query_zone_solid'access);

				when HATCHED =>
					update_element (
						container	=> module.board.conductors_floating.zones.hatched,
						position	=> segment.zone_hatched,
						process		=> query_zone_hatched'access);
			end case;

		end query_module;
		
		
	begin
		log (text => "module " & to_string (module_cursor)
			& " modifying status of "
			& to_string (segment.segment)
			& " / " & to_string (operation),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end modify_status;


	


	procedure propose_segments_net (
		module_cursor	: in pac_generic_modules.cursor;
		catch_zone		: in type_catch_zone;
		layer			: in type_signal_layer;
		count			: in out natural;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use pac_contours;
			use pac_segments;
			use pac_route_solid;
			use pac_route_hatched;


			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is

				procedure query_segment (segment : in out type_segment) is begin
					if in_catch_zone (catch_zone, segment) then
						set_proposed (segment);
						count := count + 1;
						log (text => to_string (segment), level => log_threshold + 1);
					end if;
				end query_segment;

				
				procedure query_zone_solid (zone : in out type_route_solid) is
					sc : pac_segments.cursor;
				begin
					if zone.properties.layer = layer then
						if is_circular (zone) then
							null; -- CS
						else
							sc := zone.contour.segments.first;

							while sc /= pac_segments.no_element loop
								update_element (zone.contour.segments, sc, query_segment'access);
								next (sc);
							end loop;
						end if;
					end if;
				end query_zone_solid;

				
				procedure query_zone_hatched (zone : in out type_route_hatched) is
					sc : pac_segments.cursor;
				begin
					if zone.properties.layer = layer then
						if is_circular (zone) then
							null; -- CS
						else
							sc := zone.contour.segments.first;

							while sc /= pac_segments.no_element loop
								update_element (zone.contour.segments, sc, query_segment'access);
								next (sc);
							end loop;
						end if;
					end if;
				end query_zone_hatched;

				
				zcs : pac_route_solid.cursor := net.route.zones.solid.first;
				zch : pac_route_hatched.cursor := net.route.zones.hatched.first;
				
			begin
				while zcs /= pac_route_solid.no_element loop
					update_element (net.route.zones.solid, zcs, query_zone_solid'access);
					next (zcs);
				end loop;
				
				while zch /= pac_route_hatched.no_element loop
					update_element (net.route.zones.hatched, zch, query_zone_hatched'access);
					next (zch);
				end loop;
			end query_net;
			
			
			nc : pac_nets.cursor := module.nets.first;
			
		begin
			while nc /= pac_nets.no_element loop
				update_element (module.nets, nc, query_net'access);				
				next (nc);
			end loop;
		end query_module;
	
		
	begin
		log (text => "module " & to_string (module_cursor)
			 & " proposing segments of connected zones in"
			 & " signal layer " & to_string (layer)
			 & " in " & to_string (catch_zone),
			 level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end propose_segments_net;






	procedure propose_segments_floating (
		module_cursor	: in pac_generic_modules.cursor;
		catch_zone		: in type_catch_zone;
		layer			: in type_signal_layer;
		count			: in out natural;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use pac_contours;
			use pac_segments;
			use pac_floating_solid;
			use pac_floating_hatched;


			procedure query_segment (segment : in out type_segment) is begin
				if in_catch_zone (catch_zone, segment) then
					set_proposed (segment);
					count := count + 1;
					log (text => to_string (segment), level => log_threshold + 1);
				end if;
			end query_segment;

			
			procedure query_zone_solid (zone : in out type_floating_solid) is
				sc : pac_segments.cursor;
			begin
				if zone.properties.layer = layer then
					if is_circular (zone) then
						null; -- CS
					else
						sc := zone.contour.segments.first;

						while sc /= pac_segments.no_element loop
							update_element (zone.contour.segments, sc, query_segment'access);
							next (sc);
						end loop;
					end if;
				end if;
			end query_zone_solid;

			
			procedure query_zone_hatched (zone : in out type_floating_hatched) is
				sc : pac_segments.cursor;
			begin
				if zone.properties.layer = layer then
					if is_circular (zone) then
						null; -- CS
					else
						sc := zone.contour.segments.first;

						while sc /= pac_segments.no_element loop
							update_element (zone.contour.segments, sc, query_segment'access);
							next (sc);
						end loop;
					end if;
				end if;
			end query_zone_hatched;

				
			zcs : pac_floating_solid.cursor := module.board.conductors_floating.zones.solid.first;
			zch : pac_floating_hatched.cursor := module.board.conductors_floating.zones.hatched.first;
				
		begin
			while zcs /= pac_floating_solid.no_element loop
				update_element (module.board.conductors_floating.zones.solid, zcs, query_zone_solid'access);
				next (zcs);
			end loop;
			
			while zch /= pac_floating_hatched.no_element loop
				update_element (module.board.conductors_floating.zones.hatched, zch, query_zone_hatched'access);
				next (zch);
			end loop;			
		end query_module;
	
		
	begin
		log (text => "module " & to_string (module_cursor)
			 & " proposing segments of floating zones in"
			 & " signal layer " & to_string (layer)
			 & " in " & to_string (catch_zone),
			 level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end propose_segments_floating;


	
	

	

	procedure reset_proposed_segments_net (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is


		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use pac_contours;
			use pac_segments;
			use pac_route_solid;
			use pac_route_hatched;


			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is

				procedure query_segment (segment : in out type_segment) is begin
					reset_status (segment);
				end query_segment;

				
				procedure query_zone_solid (zone : in out type_route_solid) is
					sc : pac_segments.cursor;
				begin
					if is_circular (zone) then
						null; -- CS
					else
						sc := zone.contour.segments.first;

						while sc /= pac_segments.no_element loop
							update_element (zone.contour.segments, sc, query_segment'access);
							next (sc);
						end loop;
					end if;
				end query_zone_solid;

				
				procedure query_zone_hatched (zone : in out type_route_hatched) is
					sc : pac_segments.cursor;
				begin
					if is_circular (zone) then
						null; -- CS
					else
						sc := zone.contour.segments.first;

						while sc /= pac_segments.no_element loop
							update_element (zone.contour.segments, sc, query_segment'access);
							next (sc);
						end loop;
					end if;
				end query_zone_hatched;

				
				zcs : pac_route_solid.cursor := net.route.zones.solid.first;
				zch : pac_route_hatched.cursor := net.route.zones.hatched.first;
				
			begin
				while zcs /= pac_route_solid.no_element loop
					update_element (net.route.zones.solid, zcs, query_zone_solid'access);
					next (zcs);
				end loop;
				
				while zch /= pac_route_hatched.no_element loop
					update_element (net.route.zones.hatched, zch, query_zone_hatched'access);
					next (zch);
				end loop;
			end query_net;
			
			
			nc : pac_nets.cursor := module.nets.first;
			
		begin
			while nc /= pac_nets.no_element loop
				update_element (module.nets, nc, query_net'access);				
				next (nc);
			end loop;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " resetting proposed segments of connected zones in conductor layers",
			 level => log_threshold);

		log_indentation_up;

		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;		
	end reset_proposed_segments_net;


	


	

	procedure reset_proposed_segments_floating (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is


		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use pac_contours;
			use pac_segments;
			use pac_floating_solid;
			use pac_floating_hatched;


			procedure query_segment (segment : in out type_segment) is begin
				reset_status (segment);
			end query_segment;

			
			procedure query_zone_solid (zone : in out type_floating_solid) is
				sc : pac_segments.cursor;
			begin
				if is_circular (zone) then
					null; -- CS
				else
					sc := zone.contour.segments.first;

					while sc /= pac_segments.no_element loop
						update_element (zone.contour.segments, sc, query_segment'access);
						next (sc);
					end loop;
				end if;
			end query_zone_solid;

			
			procedure query_zone_hatched (zone : in out type_floating_hatched) is
				sc : pac_segments.cursor;
			begin
				if is_circular (zone) then
					null; -- CS
				else
					sc := zone.contour.segments.first;

					while sc /= pac_segments.no_element loop
						update_element (zone.contour.segments, sc, query_segment'access);
						next (sc);
					end loop;
				end if;
			end query_zone_hatched;

			
			zcs : pac_floating_solid.cursor := module.board.conductors_floating.zones.solid.first;
			zch : pac_floating_hatched.cursor := module.board.conductors_floating.zones.hatched.first;
				
		begin
			while zcs /= pac_floating_solid.no_element loop
				update_element (module.board.conductors_floating.zones.solid, zcs, query_zone_solid'access);
				next (zcs);
			end loop;
			
			while zch /= pac_floating_hatched.no_element loop
				update_element (module.board.conductors_floating.zones.hatched, zch, query_zone_hatched'access);
				next (zch);
			end loop;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " resetting proposed segments of floating zones in conductor layers",
			 level => log_threshold);

		log_indentation_up;

		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;		
	end reset_proposed_segments_floating;

	

	

	

	function get_first_segment_net (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;								 
		log_threshold	: in type_log_level)
		return type_object_segment_net
	is

		result : type_object_segment_net;
		-- Note: By default the fill style of the result is SOLID.
		-- However, in the end of this procedure it may mutate 
		-- to HATCHED. See specification.

		use pac_contours;
		use pac_segments;
		use pac_route_solid;
		use pac_route_hatched;

		-- The segment cursor:
		sc : pac_segments.cursor;

		-- The net cursor:
		net_cursor : pac_nets.cursor;

		-- Then zone cursors (one for solid, another for hatched):
		zcs : pac_route_solid.cursor;
		zch : pac_route_hatched.cursor;

		-- This flag is used to abort the iterators for nets, zones and segments
		-- as soon as a segment has been found:
		proceed : boolean := true;
		
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is			

			
			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in type_net) 
			is
				
				procedure query_segment (segment : in type_segment) is begin
					case flag is
						when PROPOSED =>
							if is_proposed (segment) then
								proceed := false;
								log (text => to_string (segment), level => log_threshold + 3);
							end if;

						when SELECTED =>
							if is_selected (segment) then
								proceed := false;
								log (text => to_string (segment), level => log_threshold + 3);
							end if;

						when others =>
							null; -- CS
					end case;
				end query_segment;

				
				procedure query_zone_solid (zone : in type_route_solid) is begin
					if is_circular (zone) then
						null; -- CS
					else
						sc := zone.contour.segments.first;

						while sc /= pac_segments.no_element loop
							query_element (sc, query_segment'access);
							if not proceed then
								exit;
							end if;
							next (sc);
						end loop;
					end if;
				end query_zone_solid;

				
				procedure query_zone_hatched (zone : in type_route_hatched) is begin
					if is_circular (zone) then
						null; -- CS
					else
						sc := zone.contour.segments.first;

						while sc /= pac_segments.no_element loop
							query_element (sc, query_segment'access);
							if not proceed then
								exit;
							end if;
							next (sc);
						end loop;
					end if;
				end query_zone_hatched;

				
			begin
				log_indentation_up;
				log (text => to_string (net_name), level => log_threshold + 2);
				log_indentation_up;
				
				-- First search among solidly filled zones:
				zcs := net.route.zones.solid.first;				
				while zcs /= pac_route_solid.no_element loop
					query_element (zcs, query_zone_solid'access);
					if not proceed then
						exit;
					end if;
					next (zcs);
				end loop;

				-- If nothing found, search among hatched zones:
				if proceed then
					zch := net.route.zones.hatched.first;
					while zch /= pac_route_hatched.no_element loop
						query_element (zch, query_zone_hatched'access);
						if not proceed then
							exit;
						end if;
						next (zch);
					end loop;
				end if;

				log_indentation_down;
				log_indentation_down;
			end query_net;

			
		begin
			log (text => "nets", level => log_threshold + 1);
			net_cursor := module.nets.first;
			while net_cursor /= pac_nets.no_element loop
				query_element (net_cursor, query_net'access);
				if not proceed then
					exit;
				end if;
				next (net_cursor);
			end loop;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " looking up the first segment of a connected zone / " & to_string (flag),
			level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_module'access);

		-- put_line ("found " & to_string (result));
		
		log_indentation_down;

		if not proceed then -- a segment has been found.
			-- The segment is either belonging to a solid
			-- or a hatche zone:
			
			if has_element (zcs) then
				result := (
					fill_style 		=> SOLID,
					zone_solid		=> zcs,
					segment			=> sc,
					net				=> net_cursor);

			elsif has_element (zch) then
				result := (
					fill_style 		=> HATCHED, -- mutates the result !
					zone_hatched	=> zch,
					segment			=> sc,
					net				=> net_cursor);

			else
				return result; -- CS should never happen
			end if;
		else
			-- If nothing found. Return default result. See specs:
			return result;
		end if;
		
		return result;
	end get_first_segment_net;






	function get_first_segment_floating (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;								 
		log_threshold	: in type_log_level)
		return type_object_segment_floating
	is

		result : type_object_segment_floating;
		-- Note: By default the fill style of the result is SOLID.
		-- However, in the end of this procedure it may mutate 
		-- to HATCHED. See specification.

		use pac_contours;
		use pac_segments;
		use pac_floating_solid;
		use pac_floating_hatched;

		-- The segment cursor:
		sc : pac_segments.cursor;

		-- Then zone cursors (one for solid, another for hatched):
		zcs : pac_floating_solid.cursor;
		zch : pac_floating_hatched.cursor;

		-- This flag is used to abort the iterators zones and segments
		-- as soon as a segment has been found:
		proceed : boolean := true;
		
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is			

			procedure query_segment (segment : in type_segment) is begin
				case flag is
					when PROPOSED =>
						if is_proposed (segment) then
							proceed := false;
							log (text => to_string (segment), level => log_threshold + 3);
						end if;

					when SELECTED =>
						if is_selected (segment) then
							proceed := false;
							log (text => to_string (segment), level => log_threshold + 3);
						end if;

					when others =>
						null; -- CS
				end case;
			end query_segment;

			
			procedure query_zone_solid (zone : in type_floating_solid) is begin
				if is_circular (zone) then
					null; -- CS
				else
					sc := zone.contour.segments.first;

					while sc /= pac_segments.no_element loop
						query_element (sc, query_segment'access);
						if not proceed then
							exit;
						end if;
						next (sc);
					end loop;
				end if;
			end query_zone_solid;

			
			procedure query_zone_hatched (zone : in type_floating_hatched) is begin
				if is_circular (zone) then
					null; -- CS
				else
					sc := zone.contour.segments.first;

					while sc /= pac_segments.no_element loop
						query_element (sc, query_segment'access);
						if not proceed then
							exit;
						end if;
						next (sc);
					end loop;
				end if;
			end query_zone_hatched;

				
		begin
			log_indentation_up;
			
			-- First search among solidly filled zones:
			zcs := module.board.conductors_floating.zones.solid.first;				
			while zcs /= pac_floating_solid.no_element loop
				query_element (zcs, query_zone_solid'access);
				if not proceed then
					exit;
				end if;
				next (zcs);
			end loop;

			-- If nothing found, search among hatched zones:
			if proceed then
				zch := module.board.conductors_floating.zones.hatched.first;
				while zch /= pac_floating_hatched.no_element loop
					query_element (zch, query_zone_hatched'access);
					if not proceed then
						exit;
					end if;
					next (zch);
				end loop;
			end if;

			log_indentation_down;

		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " looking up the first segment of a floating zone / " & to_string (flag),
			level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_module'access);

		-- put_line ("found " & to_string (result));
		
		log_indentation_down;

		if not proceed then -- a segment has been found.
			-- The segment is either belonging to a solid
			-- or a hatche zone:
			
			if has_element (zcs) then
				result := (
					fill_style 		=> SOLID,
					zone_solid		=> zcs,
					segment			=> sc);

			elsif has_element (zch) then
				result := (
					fill_style 		=> HATCHED, -- mutates the result !
					zone_hatched	=> zch,
					segment			=> sc);

			else
				return result; -- CS should never happen
			end if;
		else
			-- If nothing found. Return default result. See specs:
			return result;
		end if;
		
		return result;
	end get_first_segment_floating;

	


	

	procedure move_segment_net (
		module_cursor	: in pac_generic_modules.cursor;
		segment			: in type_object_segment_net;
		point_of_attack	: in type_vector_model;
		-- coordinates		: in type_coordinates; -- relative/absolute
		destination		: in type_vector_model;
		log_threshold	: in type_log_level)
	is
		use pac_contours;
		use pac_segments;
		use pac_route_solid;
		use pac_route_hatched;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is

			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is

				-- Moves the candidate segment:
				procedure query_segment (s : in out type_segment) is begin
					move_segment (s, point_of_attack, destination);
				end query_segment;

				
				procedure query_zone_solid (zone : in out type_route_solid) is begin
					if is_circular (zone) then
						null; -- CS
					else
						-- Locate the given segment:
						update_element (zone.contour.segments, segment.segment, query_segment'access);
					end if;
				end query_zone_solid;

				
				procedure query_zone_hatched (zone : in out type_route_hatched) is begin
					if is_circular (zone) then
						null; -- CS
					else
						-- Locate the given segment:
						update_element (zone.contour.segments, segment.segment, query_segment'access);
					end if;
				end query_zone_hatched;

				
			begin
				-- Locate the zone as given by the segment:
				case segment.fill_style is
					when SOLID =>
						update_element (net.route.zones.solid, segment.zone_solid, query_zone_solid'access);

					when HATCHED =>
						update_element (net.route.zones.hatched, segment.zone_hatched, query_zone_hatched'access);
				end case;
			end query_net;

				
		begin
			-- Locate the net given by the segment:
			update_element (module.nets, segment.net, query_net'access);
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " moving zone segment " & to_string (segment.segment)
			& " of net " & to_string (segment.net)
			& " point of attack " & to_string (point_of_attack)
			& " to" & to_string (destination),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (						
			position	=> module_cursor,
			process		=> query_module'access);
		
		log_indentation_down;
	end move_segment_net;






	procedure move_segment_floating (
		module_cursor	: in pac_generic_modules.cursor;
		segment			: in type_object_segment_floating;
		point_of_attack	: in type_vector_model;
		-- coordinates		: in type_coordinates; -- relative/absolute
		destination		: in type_vector_model;
		log_threshold	: in type_log_level)
	is
		use pac_contours;
		use pac_segments;
		use pac_floating_solid;
		use pac_floating_hatched;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is

			-- Moves the candidate segment:
			procedure query_segment (s : in out type_segment) is begin
				move_segment (s, point_of_attack, destination);
			end query_segment;

			
			procedure query_zone_solid (zone : in out type_floating_solid) is begin
				if is_circular (zone) then
					null; -- CS
				else
					-- Locate the given segment:
					update_element (zone.contour.segments, segment.segment, query_segment'access);
				end if;
			end query_zone_solid;

			
			procedure query_zone_hatched (zone : in out type_floating_hatched) is begin
				if is_circular (zone) then
					null; -- CS
				else
					-- Locate the given segment:
					update_element (zone.contour.segments, segment.segment, query_segment'access);
				end if;
			end query_zone_hatched;

				
		begin
			-- Locate the zone as given by the segment:
			case segment.fill_style is
				when SOLID =>
					update_element (module.board.conductors_floating.zones.solid, segment.zone_solid, query_zone_solid'access);

				when HATCHED =>
					update_element (module.board.conductors_floating.zones.hatched, segment.zone_hatched, query_zone_hatched'access);
			end case;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " moving floating zone segment " & to_string (segment.segment)
			& " point of attack " & to_string (point_of_attack)
			& " to" & to_string (destination),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (						
			position	=> module_cursor,
			process		=> query_module'access);
		
		log_indentation_down;
	end move_segment_floating;


	


	

	procedure delete_segment_net (
		module_cursor	: in pac_generic_modules.cursor;
		segment			: in type_object_segment_net;
		log_threshold	: in type_log_level)
	is
		use pac_contours;
		use pac_segments;
		use pac_route_solid;
		use pac_route_hatched;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is

			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is
				
				procedure query_zone_solid (zone : in out type_route_solid) is 
					c : pac_segments.cursor := segment.segment;
				begin
					if is_circular (zone) then
						null; -- CS
					else
						-- Delete the given segment:
						delete (zone.contour.segments, c);
					end if;
				end query_zone_solid;

				
				procedure query_zone_hatched (zone : in out type_route_hatched) is 
					c : pac_segments.cursor := segment.segment;
				begin
					if is_circular (zone) then
						null; -- CS
					else
						-- Delete the given segment:
						delete (zone.contour.segments, c);
					end if;
				end query_zone_hatched;

				
			begin
				-- Locate the zone as given by the segment:
				case segment.fill_style is
					when SOLID =>
						update_element (net.route.zones.solid, segment.zone_solid, query_zone_solid'access);

					when HATCHED =>
						update_element (net.route.zones.hatched, segment.zone_hatched, query_zone_hatched'access);
				end case;
			end query_net;

				
		begin
			-- Locate the net given by the segment:
			update_element (module.nets, segment.net, query_net'access);
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " deleting zone segment " & to_string (segment.segment)
			& " of net " & to_string (segment.net),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (						
			position	=> module_cursor,
			process		=> query_module'access);
		
		log_indentation_down;
	end delete_segment_net;






	procedure delete_segment_floating (
		module_cursor	: in pac_generic_modules.cursor;
		segment			: in type_object_segment_floating;
		log_threshold	: in type_log_level)
	is
		use pac_contours;
		use pac_segments;
		use pac_floating_solid;
		use pac_floating_hatched;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is

				
			procedure query_zone_solid (zone : in out type_floating_solid) is 
				c : pac_segments.cursor := segment.segment;
			begin
				if is_circular (zone) then
					null; -- CS
				else
					-- Delete the given segment:
					delete (zone.contour.segments, c);
				end if;
			end query_zone_solid;

			
			procedure query_zone_hatched (zone : in out type_floating_hatched) is 
				c : pac_segments.cursor := segment.segment;
			begin
				if is_circular (zone) then
					null; -- CS
				else
					-- Delete the given segment:
					delete (zone.contour.segments, c);
				end if;
			end query_zone_hatched;

				
		begin
			-- Locate the zone as given by the segment:
			case segment.fill_style is
				when SOLID =>
					update_element (module.board.conductors_floating.zones.solid, segment.zone_solid, query_zone_solid'access);

				when HATCHED =>
					update_element (module.board.conductors_floating.zones.hatched, segment.zone_hatched, query_zone_hatched'access);
			end case;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " deleting floating zone segment " & to_string (segment.segment),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (						
			position	=> module_cursor,
			process		=> query_module'access);
		
		log_indentation_down;
	end delete_segment_floating;

	
	
	

	procedure fill_zones (
		module_cursor	: in pac_generic_modules.cursor;
		log_category	: in type_log_category;
		log_threshold	: in type_log_level;
		nets 			: in pac_net_names.list := no_net_names)
	is separate;
	


	procedure clear_zones (
		module_cursor	: in pac_generic_modules.cursor;	
		log_threshold	: in type_log_level;
		nets 			: in pac_net_names.list := no_net_names)
	is separate;
	



	procedure add_text (
		module_cursor	: in pac_generic_modules.cursor;
		signal_layer	: in type_signal_layer;
		text			: in type_text_fab_with_content;
		log_threshold	: in type_log_level)
	is
		use et_conductor_text.boards;
		use et_mirroring;
		

		procedure place_text (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use pac_conductor_texts;
			vectors : pac_character_lines.list;
			use et_text;
			mirror : type_mirror;

			v_text : type_vector_text;
			c_text : type_conductor_text;
			
		begin
			mirror := signal_layer_to_mirror (signal_layer, get_deepest_conductor_layer (module_cursor));

			if mirror = MIRROR_ALONG_Y_AXIS then
				log (text => "text is in deepest signal layer -> will be mirrored", level => log_threshold + 1);
			else
				log (text => "text is not in deepest signal layer -> no mirroring", level => log_threshold + 1);
			end if;

			
			v_text := vectorize_text (
				content		=> text.content,
				size		=> text.size,
				rotation	=> get_rotation (text.position),
				position	=> text.position.place,
				mirror		=> mirror,
				line_width	=> text.line_width,
				make_border	=> true -- CS should be false for restrict layers
				-- CS alignment
				); 

			-- assemble the conductor text:
			c_text := (text with 
				layer		=> signal_layer,
				vectors		=> v_text -- CS call vectorize_text here directly
				--segments	=> make_segments (v_text, text.line_width)
				);
			
			append (module.board.conductors_floating.texts, c_text);
		end place_text;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " placing text in conductor layer at"
			& to_string (text.position)
			& " signal layer " & to_string (signal_layer),
			level => log_threshold);

		log_indentation_up;
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> place_text'access);

		log_indentation_down;
	end add_text;

	
	
	

	function get_texts (
		module_cursor	: in pac_generic_modules.cursor;
		catch_zone		: in type_catch_zone;
		log_threshold	: in type_log_level)
		return pac_conductor_texts.list
	is
		use et_text;
		use pac_conductor_texts;
		result : pac_conductor_texts.list;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			procedure query_text (c : in pac_conductor_texts.cursor) is
				text : type_conductor_text renames element (c);
			begin
				if in_catch_zone (
					zone	=> catch_zone,
					point	=> get_place (text))
				then
					log (text => to_string (get_place (text)) 
						& " content " & enclose_in_quotes (to_string (text.content)),
						level => log_threshold + 2);
						
					result.append (text);
				end if;
			end query_text;

			
		begin
			module.board.conductors_floating.texts.iterate (query_text'access);
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " looking up conductor texts"
			& " in" & to_string (catch_zone),
			level => log_threshold);
		
		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log (text => "found" & count_type'image (result.length),
			 level => log_threshold + 1);
		
		log_indentation_down;
		return result;
	end get_texts;



	


	procedure move_text (
		module_cursor	: in pac_generic_modules.cursor;
		text			: in type_conductor_text;
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in type_vector_model;
		log_threshold	: in type_log_level)
	is
		old_position : constant type_vector_model := get_place (text);
		new_position : type_vector_model;
		offset : type_distance_relative;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is
			text_cursor : pac_conductor_texts.cursor;

			procedure query_text (text : in out type_conductor_text) is begin
				move_text (text, offset); -- incl. vector text
			end query_text;
			
		begin
			text_cursor := module.board.conductors_floating.texts.find (text);
			module.board.conductors_floating.texts.update_element (text_cursor, query_text'access);
		end query_module;

		
	begin
		case coordinates is
			when ABSOLUTE =>
				new_position := point;
				offset := get_distance_relative (old_position, new_position);

			when RELATIVE =>
				new_position := point;
				offset := to_distance_relative (point);
				move_by (new_position, offset);
		end case;
		
		log (text => "module " & to_string (module_cursor)
			& " moving conductor text from" & to_string (old_position)
			& " to" & to_string (new_position), -- CS by offset, signal layer number
			level => log_threshold);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);

	end move_text;



	

	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		text			: in type_object_text;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is

			procedure query_text (text : in out type_conductor_text) is begin
				modify_status (text, operation);
			end query_text;
			
		begin
			module.board.conductors_floating.texts.update_element (
				text.cursor, query_text'access);

		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " modifying status of text" -- CS log position and content ?
			& " / " & to_string (operation),
			level => log_threshold);

		log_indentation_up;

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);
		
		log_indentation_down;
	end modify_status;






	procedure propose_texts (
		module_cursor	: in pac_generic_modules.cursor;
		layer			: in type_signal_layer;
		catch_zone		: in type_catch_zone;
		count			: in out natural;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use pac_conductor_texts;
			c : pac_conductor_texts.cursor := module.board.conductors_floating.texts.first;

			procedure query_text (
				text	: in out type_conductor_text)
			is begin
				-- The candidate text must be in the given signal layer:
				if get_layer (text) = layer then
					
					if in_catch_zone (
						zone	=> catch_zone,
						point	=> get_place (text))
					then
						set_proposed (text);
						count := count + 1;
						log (text => to_string (text), level => log_threshold + 1);
					end if;
					
				end if;
			end query_text;
			
			
		begin
			while c /= pac_conductor_texts.no_element loop
				module.board.conductors_floating.texts.update_element (c, query_text'access);
				next (c);
			end loop;
		end query_module;
		
		
	begin
		log (text => "module " & to_string (module_cursor)
			 & " proposing texts in layer " & to_string (layer)
			 & " in" & to_string (catch_zone),
			 level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end propose_texts;






	procedure move_text (
		module_cursor	: in pac_generic_modules.cursor;
		text			: in type_object_text;
		destination		: in type_vector_model;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is

			procedure query_text (text : in out type_conductor_text) is begin
				move_text (text, destination);
			end query_text;
			
		begin
			module.board.conductors_floating.texts.update_element (
				text.cursor, query_text'access);

		end query_module;
		

	begin
		log (text => "module " & to_string (module_cursor)
			& " moving conductor text " & to_string (text.cursor)
			& " " & to_string (destination),
			level => log_threshold);

		log_indentation_up;

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);
		
		log_indentation_down;
	end move_text;

	


	procedure delete_text (
		module_cursor	: in pac_generic_modules.cursor;
		text			: in type_object_text;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is
			c : pac_conductor_texts.cursor := text.cursor;			
		begin
			module.board.conductors_floating.texts.delete (c);
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " deleting conductor text " & to_string (text.cursor),
			level => log_threshold);

		log_indentation_up;

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);
		
		log_indentation_down;
	end delete_text;


	



	function get_first_text (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;								 
		log_threshold	: in type_log_level)
		return type_object_text
	is
		result : type_object_text;


		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			use pac_conductor_texts;
			
			proceed : aliased boolean := true;

			texts : pac_conductor_texts.list renames module.board.conductors_floating.texts;

			
			procedure query_text (c : in pac_conductor_texts.cursor) is
				use et_object_status;
			begin
				case flag is
					when PROPOSED =>
						if is_proposed (c) then
							result.cursor := c;
							proceed := false;
						end if;

					when SELECTED =>
						if is_selected (c) then
							result.cursor := c;
							proceed := false;
						end if;

					when others =>
						null; -- CS
				end case;
			end query_text;
	
			
		begin
			-- Query the texts:
			iterate (texts, query_text'access, proceed'access);

			-- If nothing found, return no_element:
			if proceed then
				result := (others => <>);	
			end if;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " looking up the first text / " & to_string (flag),
			level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_module'access);

		-- put_line ("found " & to_string (result));
		
		log_indentation_down;

		return result;
	end get_first_text;

	


	

	procedure reset_proposed_texts (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			
			procedure query_text (
				text	: in out type_conductor_text)
			is begin
				reset_status (text);
			end query_text;

			use pac_conductor_texts;
			c : pac_conductor_texts.cursor := module.board.conductors_floating.texts.first;
		begin
			while c /= pac_conductor_texts.no_element loop
				module.board.conductors_floating.texts.update_element (
					c, query_text'access);
				next (c);
			end loop;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			 & " resetting proposed texts",
			 level => log_threshold);

		log_indentation_up;

		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end reset_proposed_texts;





	procedure add_placeholder (
		module_cursor	: in pac_generic_modules.cursor;
		placeholder		: in type_text_placeholder_conductors;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use pac_text_placeholders_conductors;
		begin
			append (module.board.conductors_floating.placeholders, placeholder);
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " placing text placeholder in conductor layer "
			& to_string (placeholder),
			level => log_threshold);

		log_indentation_up;
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end add_placeholder;

	



	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		placeholder		: in type_object_placeholder;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is

			procedure query_placeholder (
				ph : in out type_text_placeholder_conductors) 
			is begin
				modify_status (ph, operation);
			end query_placeholder;
			
		begin
			module.board.conductors_floating.placeholders.update_element (
				placeholder.cursor, query_placeholder'access);

		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " modifying status of text placeholder" -- CS log position and content ?
			& " / " & to_string (operation),
			level => log_threshold);

		log_indentation_up;

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);
		
		log_indentation_down;
	end modify_status;



	


	procedure propose_placeholders (
		module_cursor	: in pac_generic_modules.cursor;
		layer			: in type_signal_layer;
		catch_zone		: in type_catch_zone;
		count			: in out natural;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use pac_text_placeholders_conductors;
			c : pac_text_placeholders_conductors.cursor := module.board.conductors_floating.placeholders.first;

			procedure query_placeholder (
				ph : in out type_text_placeholder_conductors)
			is begin
				if in_catch_zone (
					zone	=> catch_zone,
					point	=> get_place (ph))
				then
					set_proposed (ph);
					count := count + 1;
					log (text => to_string (ph), level => log_threshold + 1);
				end if;
			end query_placeholder;
			
			
		begin
			while c /= pac_text_placeholders_conductors.no_element loop
				module.board.conductors_floating.placeholders.update_element (c, query_placeholder'access);
				next (c);
			end loop;
		end query_module;
		
		
	begin
		log (text => "module " & to_string (module_cursor)
			 & " proposing text placeholders in layer " & to_string (layer)
			 & " in" & to_string (catch_zone),
			 level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end propose_placeholders;



	


	procedure move_placeholder (
		module_cursor	: in pac_generic_modules.cursor;
		placeholder		: in type_object_placeholder;
		destination		: in type_vector_model;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is

			procedure query_placeholder (
				ph : in out type_text_placeholder_conductors) 
			is begin
				move_text (ph, destination);
			end query_placeholder;
			
		begin
			module.board.conductors_floating.placeholders.update_element (
				placeholder.cursor, query_placeholder'access);

		end query_module;
		

	begin
		log (text => "module " & to_string (module_cursor)
			& " moving conductor text placeholder " 
			& to_string (placeholder.cursor)
			& " " & to_string (destination),
			level => log_threshold);

		log_indentation_up;

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);
		
		log_indentation_down;
	end move_placeholder;



	


	procedure delete_placeholder (
		module_cursor	: in pac_generic_modules.cursor;
		placeholder		: in type_object_placeholder;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is
			c : pac_text_placeholders_conductors.cursor := placeholder.cursor;			
		begin
			module.board.conductors_floating.placeholders.delete (c);
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " deleting conductor text placeholder" & to_string (placeholder.cursor),
			level => log_threshold);

		log_indentation_up;

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);
		
		log_indentation_down;
	end delete_placeholder;


	



	function get_first_placeholder (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;								 
		log_threshold	: in type_log_level)
		return type_object_placeholder
	is
		result : type_object_placeholder;


		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			use pac_text_placeholders_conductors;
			
			proceed : aliased boolean := true;

			phs : pac_text_placeholders_conductors.list 
				renames module.board.conductors_floating.placeholders;

			
			procedure query_placeholder (
				c : in pac_text_placeholders_conductors.cursor) 
			is
				use et_object_status;
			begin
				case flag is
					when PROPOSED =>
						if is_proposed (c) then
							result.cursor := c;
							proceed := false;
						end if;

					when SELECTED =>
						if is_selected (c) then
							result.cursor := c;
							proceed := false;
						end if;

					when others =>
						null; -- CS
				end case;
			end query_placeholder;
	
			
		begin
			-- Query the placeholders:
			iterate (phs, query_placeholder'access, proceed'access);

			-- If nothing found, return no_element:
			if proceed then
				result := (others => <>);	
			end if;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " looking up the first text placeholder / " & to_string (flag),
			level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_module'access);

		-- put_line ("found " & to_string (result));
		
		log_indentation_down;

		return result;
	end get_first_placeholder;

	


	

	procedure reset_proposed_placeholders (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			
			procedure query_placeholder (
				ph : in out type_text_placeholder_conductors)
			is begin
				reset_status (ph);
			end query_placeholder;

			use pac_text_placeholders_conductors;
			c : pac_text_placeholders_conductors.cursor := 
				module.board.conductors_floating.placeholders.first;
		begin
			while c /= pac_text_placeholders_conductors.no_element loop
				module.board.conductors_floating.placeholders.update_element (
					c, query_placeholder'access);
				next (c);
			end loop;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			 & " resetting proposed text placeholders",
			 level => log_threshold);

		log_indentation_up;

		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end reset_proposed_placeholders;






-- OBJECTS:
	

	function get_count (
		objects : in pac_objects.list)
		return natural
	is begin
		return natural (objects.length);
	end get_count;

	



	
	

	function get_first_object (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;								 
		log_threshold	: in type_log_level)
		return type_object
	is
		result_category			: type_object_category := CAT_VOID;
		result_segment_net  	: type_object_segment_net;
		result_segment_floating : type_object_segment_floating;
		result_line_net			: type_object_line_net;
		result_line_floating	: type_object_line_floating;
		result_arc_net			: type_object_arc_net;
		result_arc_floating		: type_object_arc_floating;
		result_text				: type_object_text;
		result_placeholder		: type_object_placeholder;

		use pac_contours;
		use pac_segments;

		use pac_conductor_lines;
		use pac_conductor_arcs;
		use pac_conductor_texts;
		use pac_text_placeholders_conductors;


		
		procedure search_for_line_of_net is begin
			result_line_net := get_first_line_net (module_cursor, flag, log_threshold + 1);
			
			if result_line_net.line_cursor /= pac_conductor_lines.no_element then
				-- A line has been found.
				log (text => to_string (element (result_line_net.line_cursor)),
					level => log_threshold + 1);
				
				result_category := CAT_LINE_NET;
			end if;

		end search_for_line_of_net;

		

		procedure search_for_floating_line is begin
			result_line_floating := get_first_line_floating (module_cursor, flag, log_threshold + 1);
			
			if result_line_floating.line_cursor /= pac_conductor_lines.no_element then
				-- A line has been found.
				log (text => to_string (element (result_line_floating.line_cursor)),
					level => log_threshold + 1);
				
				result_category := CAT_LINE_FLOATING;
			end if;
		end search_for_floating_line;


		
		procedure search_for_arc_of_net is begin
			result_arc_net := get_first_arc_net (module_cursor, flag, log_threshold + 1);
			
			if result_arc_net.arc_cursor /= pac_conductor_arcs.no_element then
				-- An arc has been found.
				log (text => to_string (element (result_arc_net.arc_cursor)),
					level => log_threshold + 1);
				
				result_category := CAT_ARC_NET;
			end if;

		end search_for_arc_of_net;


		procedure search_for_floating_arc is begin
			result_arc_floating := get_first_arc_floating (module_cursor, flag, log_threshold + 1);
			
			if result_arc_floating.arc_cursor /= pac_conductor_arcs.no_element then
				-- An arc has been found.
				log (text => to_string (element (result_arc_floating.arc_cursor)),
					level => log_threshold + 1);
				
				result_category := CAT_ARC_FLOATING;
			end if;
		end search_for_floating_arc;
	


		procedure search_for_segment_of_connected_zone is begin
			result_segment_net := get_first_segment_net (module_cursor, flag, log_threshold + 1);

			if result_segment_net.segment /= pac_segments.no_element then
				-- A segment has been found.
				log (text => to_string (result_segment_net.segment),
					-- CS face
					level => log_threshold + 1);
				
				result_category := CAT_ZONE_SEGMENT_NET;
			end if;
		end search_for_segment_of_connected_zone;
	


		procedure search_for_segment_of_floating_zone is begin
			result_segment_floating := get_first_segment_floating (module_cursor, flag, log_threshold + 1);

			if result_segment_floating.segment /= pac_segments.no_element then
				-- A segment has been found.
				log (text => to_string (result_segment_floating.segment),
					-- CS face
					level => log_threshold + 1);
				
				result_category := CAT_ZONE_SEGMENT_FLOATING;
			end if;
		end search_for_segment_of_floating_zone;
	

		procedure search_for_text is begin
			result_text := get_first_text (module_cursor, flag, log_threshold + 1);
			
			if result_text.cursor /= pac_conductor_texts.no_element then
				-- A text has been found.
				log (text => to_string (result_text.cursor),
					level => log_threshold + 1);
				
				result_category := CAT_TEXT;
			end if;
		end search_for_text;


		procedure search_for_placeholder is begin			
			result_placeholder := get_first_placeholder (module_cursor, flag, log_threshold + 1);
			
			if result_placeholder.cursor /= pac_text_placeholders_conductors.no_element then
				-- A placeholder has been found.
				log (text => to_string (result_placeholder.cursor),
					level => log_threshold + 1);
				
				result_category := CAT_PLACEHOLDER;
			end if;
		end search_for_placeholder;
	
		
	begin
		log (text => "module " & to_string (module_cursor)
			& " looking up the first object / " & to_string (flag),
			level => log_threshold);

		log_indentation_up;
		

		search_for_line_of_net;
		-- If a line has been found, then go to the end of this procedure:

		-- If an object has been found, then the search is done:
		if result_category /= CAT_VOID then
			goto end_of_search;
		end if;


		
		search_for_floating_line;
		-- If a line has been found, then go to the end of this procedure:

		-- If an object has been found, then the search is done:
		if result_category /= CAT_VOID then
			goto end_of_search;
		end if;



		search_for_arc_of_net;		
		-- If an arc has been found, then go to the end of this procedure:

		-- If an object has been found, then the search is done:
		if result_category /= CAT_VOID then
			goto end_of_search;
		end if;



		search_for_floating_arc;
		-- If an arc has been found, then go to the end of this procedure:

		-- If an object has been found, then the search is done:
		if result_category /= CAT_VOID then
			goto end_of_search;
		end if;


		
		-- Now we search for an circle.
		-- If there is one, then go to the end of this procedure:
		-- CS

		
		search_for_segment_of_connected_zone;
		-- If an object has been found, then the search is done:

		if result_category /= CAT_VOID then
			goto end_of_search;
		end if;



		search_for_segment_of_floating_zone;
		
		-- If an object has been found, then the search is done:
		if result_category /= CAT_VOID then
			goto end_of_search;
		end if;


		
		search_for_text;

		-- If an object has been found, then the search is done:
		if result_category /= CAT_VOID then
			goto end_of_search;
		end if;
		

		search_for_placeholder;

		
		-- If still nothing has been found then the category is CAT_VOID.
		

	<<end_of_search>>
		
		log_indentation_down;

		case result_category is
			when CAT_VOID =>
				return (cat => CAT_VOID);

			when CAT_LINE_NET =>
				return (CAT_LINE_NET, result_line_net);

			when CAT_ARC_NET =>
				return (CAT_ARC_NET, result_arc_net);
				
			when CAT_LINE_FLOATING =>
				return (CAT_LINE_FLOATING, result_line_floating);

			when CAT_ARC_FLOATING =>
				return (CAT_ARC_FLOATING, result_arc_floating);
				
			when CAT_ZONE_SEGMENT_NET =>
				return (CAT_ZONE_SEGMENT_NET, result_segment_net);

			when CAT_ZONE_SEGMENT_FLOATING =>
				return (CAT_ZONE_SEGMENT_FLOATING, result_segment_floating);
				
			when CAT_TEXT =>
				return (CAT_TEXT, result_text);

			when CAT_PLACEHOLDER =>
				return (CAT_PLACEHOLDER, result_placeholder);

		end case;
	end get_first_object;






	function get_objects (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;								 
		log_threshold	: in type_log_level)
		return pac_objects.list
	is
		use pac_objects;
		result : pac_objects.list;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			net_cursor : pac_nets.cursor;
			
			-- This procedure queries a net:
			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in type_net)
			is 
				
				use pac_route_solid;
				zone_cursor_solid_net : pac_route_solid.cursor;
				
				use pac_route_hatched;
				zone_cursor_hatched_net : pac_route_hatched.cursor;

				use pac_contours;
				use pac_segments;
				
				use pac_conductor_lines;
				use pac_conductor_arcs;


				-- This procedure queries a line of a net:
				procedure query_line_net (line_cursor : in pac_conductor_lines.cursor) is 

					procedure collect is begin
						result.append ((
							cat			=> CAT_LINE_NET,
							line_net	=> (net_cursor, line_cursor)));

						-- Log the line and its linewidth:
						log (text => to_string (line_cursor, true), level => log_threshold + 2);
					end collect;

					
				begin
					case flag is
						when PROPOSED =>
							if is_proposed (line_cursor) then
								collect;
							end if;

						when SELECTED =>
							if is_selected (line_cursor) then
								collect;
							end if;

						when others =>
							null; -- CS
					end case;
				end query_line_net;


				

				-- This procedure queries an arc of a net:
				procedure query_arc_net (arc_cursor : in pac_conductor_arcs.cursor) is 

					procedure collect is begin
						result.append ((
							cat		=> CAT_ARC_NET,
							arc_net	=> (net_cursor, arc_cursor)));

						-- Log the arc and its linewidth:
						log (text => to_string (arc_cursor, true), level => log_threshold + 2);
					end collect;

					
				begin
					case flag is
						when PROPOSED =>
							if is_proposed (arc_cursor) then
								collect;
							end if;

						when SELECTED =>
							if is_selected (arc_cursor) then
								collect;
							end if;

						when others =>
							null; -- CS
					end case;
				end query_arc_net;


				
				
				
				-- This procedure queries a solidly filled zone of a net:
				procedure query_zone_solid_net (zone : in type_route_solid) is
					
					procedure query_segment (segment_cursor : in pac_segments.cursor) is 
						
						procedure collect is begin
							result.append ((
								cat			=> CAT_ZONE_SEGMENT_NET,
								segment_net	=> (SOLID, segment_cursor, net_cursor, zone_cursor_solid_net)));
		
							log (text => to_string (segment_cursor), level => log_threshold + 2);
						end collect;

						
					begin
						case flag is
							when PROPOSED =>
								if is_proposed (segment_cursor) then
									collect;
								end if;
								
							when SELECTED =>
								if is_selected (segment_cursor) then
									collect;
								end if;
								
							when others => null;  -- CS
						end case;
					end query_segment;

					
				begin
					if zone.contour.circular then
						null; -- CS
					else
						-- Iterate all segments of the candidate zone:
						iterate (zone.contour.segments, query_segment'access);
					end if;
				end query_zone_solid_net;



				

				-- This procedure queries a hatched fill zone of a net:			
				procedure query_zone_hatched_net (zone : in type_route_hatched) is
					
					procedure query_segment (segment_cursor : in pac_segments.cursor) is 

						procedure collect is begin
							result.append ((
								cat			=> CAT_ZONE_SEGMENT_NET,
								segment_net	=> (HATCHED, segment_cursor, net_cursor, zone_cursor_hatched_net)));
		
							log (text => to_string (segment_cursor), level => log_threshold + 2);
						end collect;

						
					begin
						case flag is
							when PROPOSED =>
								if is_proposed (segment_cursor) then
									collect;
								end if;

							when SELECTED =>
								if is_selected (segment_cursor) then
									collect;
								end if;
								
							when others => null; -- CS
						end case;
					end query_segment;

					
				begin
					if zone.contour.circular then
						null; -- CS
					else
						-- Iterate all segments of the candidate zone:
						iterate (zone.contour.segments, query_segment'access);
					end if;
				end query_zone_hatched_net;



			begin
				log (text => to_string (net_name), level => log_threshold + 2);
				log_indentation_up;
				
				-- Iterate the lines of the net:
				iterate (net.route.lines, query_line_net'access);

				-- Iterate the arcs of the net:
				iterate (net.route.arcs, query_arc_net'access);


				-- fill zones:
				-- solid:
				log (text => "solid fill zones", level => log_threshold + 3);
				zone_cursor_solid_net := net.route.zones.solid.first;
				while zone_cursor_solid_net /= pac_route_solid.no_element loop
					query_element (zone_cursor_solid_net, query_zone_solid_net'access);
					next (zone_cursor_solid_net);
				end loop;

				-- hatched:
				log (text => "hatched fill zones", level => log_threshold + 3);
				zone_cursor_hatched_net := net.route.zones.hatched.first;
				while zone_cursor_hatched_net /= pac_route_hatched.no_element loop
					query_element (zone_cursor_hatched_net, query_zone_hatched_net'access);
					next (zone_cursor_hatched_net);
				end loop;

				log_indentation_down;
			end query_net;


			

			-- This procedure queries objects which are floating
			-- such as lines, arcs, circles, zones, texts and text placeholders:
			procedure process_floating_objects is
				use pac_contours;
				use pac_segments;

				
				-- LINES
				use pac_conductor_lines;
				
				-- This procedure queries a floating line:
				procedure query_line (c : in pac_conductor_lines.cursor) is 

					procedure collect is begin
						result.append ((
							cat				=> CAT_LINE_FLOATING,
							line_floating	=> (line_cursor => c)));
     
						-- Log the line and its linewidth:
						log (text => to_string (c, true), level => log_threshold + 2);
					end collect;

					
				begin
					case flag is
						when PROPOSED =>
							if is_proposed (c) then
								collect;
							end if;

						when SELECTED =>
							if is_selected (c) then
								collect;
							end if;

						when others =>
							null; -- CS
					end case;
				end query_line;


				
				
				-- ARCS
				use pac_conductor_arcs;

				-- This procedure queries a floating arc:
				procedure query_arc (c : in pac_conductor_arcs.cursor) is 

					procedure collect is begin
						result.append ((
							cat				=> CAT_ARC_FLOATING,
							arc_floating	=> (arc_cursor => c)));
     
						-- Log the arc and its linewidth:
						log (text => to_string (c, true), level => log_threshold + 2);
					end collect;

					
				begin
					case flag is
						when PROPOSED =>
							if is_proposed (c) then
								collect;
							end if;

						when SELECTED =>
							if is_selected (c) then
								collect;
							end if;

						when others =>
							null; -- CS
					end case;
				end query_arc;

				
				

				-- FLOATING FILL ZONES SOLID
				use pac_floating_solid;
				zcs : pac_floating_solid.cursor := module.board.conductors_floating.zones.solid.first;

				
				-- This procedure queries a floating solidly filled zone:				
				procedure query_zone_solid (zone : in type_floating_solid) is

					-- This procedure queries a contour line of the candidate zone:
					procedure query_segment (segment_cursor : in pac_segments.cursor) is 

						procedure collect is begin
							result.append ((
								cat					=> CAT_ZONE_SEGMENT_FLOATING,
								segment_floating	=> (SOLID, segment_cursor, zcs)));
		
							log (text => to_string (segment_cursor), level => log_threshold + 2);
						end collect;

						
					begin
						case flag is
							when PROPOSED =>
								if is_proposed (segment_cursor) then
									collect;
								end if;

							when SELECTED =>
								if is_selected (segment_cursor) then
									collect;
								end if;
								
							when others => null; -- CS
						end case;
					end query_segment;

					
				begin
					if zone.contour.circular then
						null; -- CS
					else
						-- Iterate all segments of the candidate zone:
						iterate (zone.contour.segments, query_segment'access);
					end if;
				end query_zone_solid;


				

				-- FLOATING FILL ZONES HATCHED
				use pac_floating_hatched;
				zch : pac_floating_hatched.cursor := module.board.conductors_floating.zones.hatched.first;

				
				-- This procedure queries a floating hatched zone:
				procedure query_zone_hatched (zone : in type_floating_hatched) is

					-- This procedure queries a contour line of the candidate zone:
					procedure query_segment (segment_cursor : in pac_segments.cursor) is 

						procedure collect is begin
							result.append ((
								cat					=> CAT_ZONE_SEGMENT_FLOATING,
								segment_floating	=> (HATCHED, segment_cursor, zch)));
		
							log (text => to_string (segment_cursor), level => log_threshold + 2);
						end collect;

						
					begin
						case flag is
							when PROPOSED =>
								if is_proposed (segment_cursor) then
									collect;
								end if;

							when SELECTED =>
								if is_selected (segment_cursor) then
									collect;
								end if;
								
							when others => null; -- CS
						end case;
					end query_segment;
					
				begin
					if zone.contour.circular then
						null; -- CS
					else
						-- Iterate all segments of the candidate zone:
						iterate (zone.contour.segments, query_segment'access);
					end if;
				end query_zone_hatched;


				

				-- TEXTS:
				use pac_conductor_texts;
				
				procedure query_text (c : in pac_conductor_texts.cursor) is begin
					-- CS test the given flag !!
					if is_proposed (c) then
						result.append ((
							cat		=> CAT_TEXT,
							text	=> (cursor => c)));
	
						log (text => to_string (c), level => log_threshold + 2);
					end if;
				end query_text;

				


				-- TEXT PLACEHOLDERS:
				use pac_text_placeholders_conductors;
				
				procedure query_placeholder (
					c : in pac_text_placeholders_conductors.cursor) 
				is begin
					-- CS test the given flag !!
					if is_proposed (c) then
						result.append ((
							cat			=> CAT_PLACEHOLDER,
							placeholder	=> (cursor => c)));
	
						log (text => to_string (c), level => log_threshold + 2);
					end if;
				end query_placeholder;

				
				
			begin
				-- Iterate all floating lines:
				iterate (module.board.conductors_floating.lines, query_line'access);

				-- Iterate all floating arcs:
				iterate (module.board.conductors_floating.arcs, query_arc'access);

				-- CS circles
				
				-- Iterate all texts:
				iterate (module.board.conductors_floating.texts, query_text'access);
	
				-- Iterate all placeholders:
				iterate (module.board.conductors_floating.placeholders, query_placeholder'access);

				
				-- Iterate all floating solidly filled zones:
				while zcs /= pac_floating_solid.no_element loop
					query_element (zcs, query_zone_solid'access);
					next (zcs);
				end loop;

				-- Iterate all floating hatched zones:
				while zch /= pac_floating_hatched.no_element loop
					query_element (zcH, query_zone_hatched'access);
					next (zch);
				end loop;
				
			end process_floating_objects;
			
		
		begin
			-- Process things connected with a net:
			log (text => "nets", level => log_threshold + 1);
			log_indentation_up;

			net_cursor := module.nets.first;
			while net_cursor /= pac_nets.no_element loop
				query_element (net_cursor, query_net'access);
				next (net_cursor);
			end loop;

			log_indentation_down;

			-- Process floating objects:
			log (text => "floating conductor objects", level => log_threshold + 1);
			log_indentation_up;
			process_floating_objects;
			log_indentation_down;			
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " looking up objects / " & to_string (flag),
			level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_module'access);
		
		log_indentation_down;
		
		return result;
	end get_objects;






	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level)
	is begin
		log (text => "module " & to_string (module_cursor)
			& " modifying status of object "
			& type_object_category'image (object.cat)
			& " / " & to_string (operation),
			level => log_threshold);

		log_indentation_up;
		
		case object.cat is
			when CAT_LINE_NET =>
				modify_status (module_cursor, object.line_net, operation, log_threshold + 1);

			when CAT_ARC_NET =>
				modify_status (module_cursor, object.arc_net, operation, log_threshold + 1);

			when CAT_LINE_FLOATING =>
				modify_status (module_cursor, object.line_floating, operation, log_threshold + 1);
				
			when CAT_ARC_FLOATING =>
				modify_status (module_cursor, object.arc_floating, operation, log_threshold + 1);

			when CAT_ZONE_SEGMENT_NET =>
				modify_status (module_cursor, object.segment_net, operation, log_threshold + 1);

			when CAT_ZONE_SEGMENT_FLOATING =>
				modify_status (module_cursor, object.segment_floating, operation, log_threshold + 1);
				
			when CAT_TEXT =>
				modify_status (module_cursor, object.text, operation, log_threshold + 1);

			when CAT_PLACEHOLDER =>
				modify_status (module_cursor, object.placeholder, operation, log_threshold + 1);
				
			when CAT_VOID =>
				null; -- CS
		end case;

		log_indentation_down;
	end modify_status;

	

	

	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		object_cursor	: in pac_objects.cursor;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level)
	is 
		use pac_objects;
		object : constant type_object := element (object_cursor);
	begin
		modify_status (module_cursor, object, operation, log_threshold);
	end modify_status;





	procedure move_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		point_of_attack	: in type_vector_model;
		-- coordinates		: in type_coordinates; -- relative/absolute
		destination		: in type_vector_model;
		log_threshold	: in type_log_level)
	is 
		use pac_conductor_lines;
		use pac_nets;
	begin
		log (text => "module " & to_string (module_cursor)
			& " moving conductor object " 
			-- CS & to_string (object)
			& " point of attack " & to_string (point_of_attack)
			& " to" & to_string (destination),
			level => log_threshold);

		log_indentation_up;

		case object.cat is
			when CAT_LINE_NET =>
				
				move_line_net (
					module_cursor	=> module_cursor, 
					line			=> object.line_net,
					point_of_attack	=> point_of_attack, 
					destination		=> destination,
					log_threshold	=> log_threshold + 1);


			when CAT_ARC_NET =>	
				
				move_arc_net (
					module_cursor	=> module_cursor, 
					arc				=> object.arc_net,
					point_of_attack	=> point_of_attack, 
					destination		=> destination,
					log_threshold	=> log_threshold + 1);

				
			when CAT_LINE_FLOATING =>

				move_line_floating (
					module_cursor	=> module_cursor, 
					line			=> object.line_floating,
					point_of_attack	=> point_of_attack, 
					destination		=> destination,
					log_threshold	=> log_threshold + 1);


			when CAT_ARC_FLOATING =>

				move_arc_floating (
					module_cursor	=> module_cursor, 
					arc				=> object.arc_floating,
					point_of_attack	=> point_of_attack, 
					destination		=> destination,
					log_threshold	=> log_threshold + 1);

				
			when CAT_ZONE_SEGMENT_NET =>
				
				move_segment_net (
					module_cursor	=> module_cursor,
					segment			=> object.segment_net,
					point_of_attack	=> point_of_attack, 
					destination		=> destination,
					log_threshold	=> log_threshold + 1);


			when CAT_ZONE_SEGMENT_FLOATING =>

				move_segment_floating (
					module_cursor	=> module_cursor,
					segment			=> object.segment_floating,
					point_of_attack	=> point_of_attack, 
					destination		=> destination,
					log_threshold	=> log_threshold + 1);

				
			when CAT_TEXT =>

				move_text (
					module_cursor	=> module_cursor,
					text			=> object.text,
					destination		=> destination,
					log_threshold	=> log_threshold + 1);


			when CAT_PLACEHOLDER =>

				move_placeholder (
					module_cursor	=> module_cursor,
					placeholder		=> object.placeholder,
					destination		=> destination,
					log_threshold	=> log_threshold + 1);

				
			when CAT_VOID =>
				null;
		end case;		
		
		log_indentation_down;
	end move_object;



	

	procedure delete_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		log_threshold	: in type_log_level)
	is 
		use pac_nets;
		use pac_conductor_lines;
		use pac_conductor_arcs;
	begin
		log (text => "module " & to_string (module_cursor)
			& " deleting conductor object",
			-- CS & to_string (object)
			level => log_threshold);

		log_indentation_up;

		case object.cat is
			when CAT_LINE_NET =>

				case ripup_mode is
					when SINGLE_SEGMENT =>
						delete_line_net (
							module_cursor	=> module_cursor, 
							net_name		=> key (object.line_net.net_cursor),			
							line			=> element (object.line_net.line_cursor),
							log_threshold	=> log_threshold + 1);					

					when WHOLE_NET =>
						ripup_net (
							module_cursor	=> active_module,
							net_name		=> key (object.line_net.net_cursor),
							log_threshold	=> log_threshold + 1);
				end case;

				

			when CAT_ARC_NET =>

				case ripup_mode is
					when SINGLE_SEGMENT =>
						delete_arc_net (
							module_cursor	=> module_cursor, 
							net_name		=> key (object.arc_net.net_cursor),			
							arc				=> element (object.arc_net.arc_cursor),
							log_threshold	=> log_threshold + 1);					

					when WHOLE_NET =>
						ripup_net (
							module_cursor	=> active_module,
							net_name		=> key (object.arc_net.net_cursor),
							log_threshold	=> log_threshold + 1);
				end case;

				
			-- CS circles

				
			when CAT_LINE_FLOATING =>

				delete_line_floating (
					module_cursor	=> module_cursor, 
					line			=> element (object.line_floating.line_cursor),
					log_threshold	=> log_threshold + 1);					



			when CAT_ARC_FLOATING =>

				delete_arc_floating (
					module_cursor	=> module_cursor, 
					arc				=> element (object.arc_floating.arc_cursor),
					log_threshold	=> log_threshold + 1);					

				
				
			when CAT_ZONE_SEGMENT_NET =>

				delete_segment_net (
					module_cursor	=> module_cursor, 
					segment			=> object.segment_net,
					log_threshold	=> log_threshold + 1);

				
			when CAT_ZONE_SEGMENT_FLOATING =>

				delete_segment_floating (
					module_cursor	=> module_cursor, 
					segment			=> object.segment_floating,
					log_threshold	=> log_threshold + 1);


				
			when CAT_TEXT =>

				delete_text (
					module_cursor	=> module_cursor, 
					text			=> object.text,
					log_threshold	=> log_threshold + 1);


			when CAT_PLACEHOLDER =>

				delete_placeholder (
					module_cursor	=> module_cursor, 
					placeholder		=> object.placeholder,
					log_threshold	=> log_threshold + 1);


				
			when CAT_VOID =>
				null;
		end case;		
		
		log_indentation_down;
	end delete_object;
	


	


	procedure reset_proposed_objects (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is begin
		log (text => "module " & to_string (module_cursor) &
			" resetting proposed objects",
			level => log_threshold);

		log_indentation_up;

		-- Nets:
		reset_proposed_lines (
			module_cursor	=> active_module, 
			freetracks		=> false,
			log_threshold	=> log_threshold + 1);

		reset_proposed_arcs (
			module_cursor	=> active_module, 
			freetracks		=> false,
			log_threshold	=> log_threshold + 1);


		
		-- Floating objects (freetracks):
		reset_proposed_lines (
			module_cursor	=> active_module, 
			freetracks		=> true,
			log_threshold	=> log_threshold + 1);

		reset_proposed_arcs (
			module_cursor	=> active_module, 
			freetracks		=> true,
			log_threshold	=> log_threshold + 1);

		
		-- CS circles

		-- zones:
		reset_proposed_segments_net (module_cursor, log_threshold + 1);
		reset_proposed_segments_floating (module_cursor, log_threshold + 1);

		-- texts and placeholders.
		reset_proposed_texts (module_cursor, log_threshold + 1);
		reset_proposed_placeholders (module_cursor, log_threshold + 1);
		
		log_indentation_down;
	end reset_proposed_objects;


	

end et_board_ops.conductors;
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
