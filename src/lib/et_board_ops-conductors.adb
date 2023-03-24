------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                     BOARD OPERATIONS / CONDUCTOR OBJECTS                 --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
--                                                                          --
--    This program is free software: you can redistribute it and/or modify  --
--    it under the terms of the GNU General Public License as published by  --
--    the Free Software Foundation, either version 3 of the License, or     --
--    (at your option) any later version.                                   --
--                                                                          --
--    This program is distributed in the hope that it will be useful,       --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of        --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         --
--    GNU General Public License for more details.                          --
--                                                                          --
--    You should have received a copy of the GNU General Public License     --
--    along with this program.  If not, see <http://www.gnu.org/licenses/>. --
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

with ada.strings;					use ada.strings;
with ada.exceptions;
with ada.tags;

with et_schematic_ops.nets;			use et_schematic_ops.nets;
with et_schematic_ops;				use et_schematic_ops;
with et_exceptions;					use et_exceptions;
with et_board_ops.devices;
with et_board_ops.ratsnest;			use et_board_ops.ratsnest;

with et_device_query_board;			use et_device_query_board;

with et_fill_zones.boards;			use et_fill_zones.boards;


package body et_board_ops.conductors is

	use pac_generic_modules;
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
		point		: in type_point; 
		accuracy	: in type_catch_zone) 
	is begin
		log (importance => WARNING, 
			 text => "no net segment found in layer" & to_string (layer) &
			 " at" & to_string (point) &
			 " in vicinity of" & catch_zone_to_string (accuracy));
	end no_net_segment_found;

	

	-- If the terminal is a THT type, then the track may start at any signal layer.
	-- If the terminal is an SMT type, then the track may start at either the top or bottom
	-- signal layer. If operator indeed whishes an inner layer a warning must be issued.
	procedure check_terminal_face_vs_layer (
		module_cursor	: in et_project.modules.pac_generic_modules.cursor;											   
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
				if layer /= layer_count (module_cursor) then
					warning;
				end if;
			end if;

		end if;
	end check_terminal_face_vs_layer;

	
	
	procedure add_named_track (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in pac_net_name.bounded_string; -- reset_n
		line			: in type_conductor_line;
		log_threshold	: in type_log_level)
	is
		procedure do_it (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
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

	begin -- add_named_track
		et_project.modules.pac_generic_modules.update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> do_it'access);

		update_ratsnest (module_cursor, log_threshold + 1);
	end add_named_track;

	
	procedure draw_track_line (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in pac_net_name.bounded_string := no_name; -- reset_n
		line			: in type_conductor_line;
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		use pac_conductor_lines;
		
		procedure add_freetrack (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is begin
			append (
				container	=> module.board.conductors.lines,
				new_item	=> line);
		end;
		
	begin -- draw_track_line
		log (text => "module " & to_string (module_name) &
			freetrack (net_name) &
			" drawing line" &
			" in layer " & to_string (line.layer) & " " &
			to_string (line),
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
			add_named_track (module_cursor, net_name, line, log_threshold + 1);
		end if;

	end draw_track_line;

	
	--procedure draw_track_line (
		--module_cursor	: in pac_generic_modules.cursor;
		--net_cursor		: in pac_nets.cursor; -- reset_n
		--line			: in type_conductor_line;
		--log_threshold	: in type_log_level) 
	--is

		--procedure add_named_track (
			--module_name	: in pac_module_name.bounded_string;
			--module		: in out type_module) 
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

	--end draw_track_line;

	
	procedure draw_track_line (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in pac_net_name.bounded_string; -- reset_n
		layer			: in et_pcb_stack.type_signal_layer;
		width			: in type_track_width;
		device			: in type_device_name;
		terminal		: in pac_terminal_name.bounded_string;
		direction		: in type_rotation;
		length			: in type_distance_positive;
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		-- This is going to be the segment we will insert. In the follwing it
		-- will be tailored according to given terminal position, direction and length.
		-- Finally it will be added to the list of line segments (via procedure add_named_track)
		-- to the given net.
		line : type_conductor_line;
		
		device_cursor : pac_devices_sch.cursor;

		
		procedure make_line (terminal_position : in type_terminal_position_fine) is begin

			-- Build the start point of the line:
			-- The start point of the line is always the x/y of the terminal.
			-- further-on set line width and layer.
			line := (
				start_point	=> to_point (terminal_position.place),
				width		=> width, -- as given by operator
				layer		=> layer, -- as given by operator
				others 		=> <>);

			check_terminal_face_vs_layer (module_cursor, terminal_position, layer);
			
			-- Build the end point of the line. It is the start point moved in direction at given length:
			--line.end_point := type_point (move (
					--point 		=> terminal_position.place,
					--direction	=> direction,
					--distance	=> length));
			line.end_point := move (
					point 		=> line.start_point,
					direction	=> direction,
					distance	=> length);
			
		end make_line;

		
	begin -- draw_track_line
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

		add_named_track (module_cursor, net_name, line, log_threshold + 1);
	end draw_track_line;


	
	procedure draw_track_line (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in pac_net_name.bounded_string; -- reset_n
		layer			: in et_pcb_stack.type_signal_layer;
		width			: in type_track_width;
		device			: in type_device_name;
		terminal		: in pac_terminal_name.bounded_string;
		direction		: in type_rotation;
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
		
		device_cursor : pac_devices_sch.cursor;

		
		procedure make_line (terminal_position : in type_terminal_position_fine) is begin

			-- Build the start point of the line:
			-- The start point of the line is always the x/y of the terminal.
			-- further-on set line width and layer.
			line := (
				start_point	=> to_point (terminal_position.place),
				width		=> width, -- as given by operator
				layer		=> layer, -- as given by operator
				others 		=> <>);

			check_terminal_face_vs_layer (module_cursor, terminal_position, layer);
			
			-- Build the end point of the line. It is the start point moved in direction:
			-- CS
			
		end make_line;

		
	begin -- draw_track_line
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

		add_named_track (module_cursor, net_name, line, log_threshold + 1);
	end draw_track_line;

	
	
	procedure draw_track_line (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in pac_net_name.bounded_string; -- reset_n
		layer			: in et_pcb_stack.type_signal_layer;
		width			: in type_track_width;
		device			: in type_device_name;
		terminal		: in pac_terminal_name.bounded_string;
		end_point		: in type_point;
		log_threshold	: in type_log_level) 
	is		
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		-- This is going to be the segment we will insert. In the follwing it
		-- will be tailored according to given terminal position and end point.
		-- Finally it will be added to the list of line segments (via procedure add_named_track)
		-- to the given net.
		line : type_conductor_line;
		
		device_cursor : pac_devices_sch.cursor;

		
		procedure make_line (terminal_position : in type_terminal_position_fine) is begin

			-- Build the start point of the line:
			-- The start point of the line is always the x/y of the terminal.
			-- further-on set line width and layer.
			line := (
				start_point	=> to_point (terminal_position.place),
				width		=> width, -- as given by operator
				layer		=> layer, -- as given by operator
				end_point	=> end_point); -- as given by operator

			check_terminal_face_vs_layer (module_cursor, terminal_position, layer);
			
		end make_line;

		
	begin -- draw_track_line
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

		add_named_track (module_cursor, net_name, line, log_threshold + 1);
	end draw_track_line;

	
	procedure draw_track_line (
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
		
		device_cursor : pac_devices_sch.cursor;
		
		procedure make_line (terminal_position : in type_terminal_position_fine) is begin

			-- Build the start point of the line:
			-- The start point of the line is always the x/y of the terminal.
			-- further-on set line width and layer.
			line := (
				start_point	=> to_point (terminal_position.place),
				width		=> width, -- as given by operator
				layer		=> layer, -- as given by operator
				others 		=> <>);

			check_terminal_face_vs_layer (module_cursor, terminal_position, layer);
			
			-- Build the end point of the line. It is the start point moved in direction:
			-- CS
			
		end make_line;

		
	begin -- draw_track_line
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

		add_named_track (module_cursor, net_name, line, log_threshold + 1);
	end draw_track_line;


	function get_lines (
		module_cursor	: in pac_generic_modules.cursor;
		layer			: in et_pcb_stack.type_signal_layer;
		point			: in type_point;
		catch_zone		: in type_catch_zone; -- the circular area around the place
		log_threshold	: in type_log_level)
		return pac_conductor_lines.list
	is
		result : pac_conductor_lines.list;
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_module) 
		is

			procedure query_line (c : in pac_conductor_lines.cursor) is
				use pac_conductor_lines;
				line : type_conductor_line renames element (c);
			begin
				if line.layer = layer then
					if in_catch_zone (
						line	=> line,
						width	=> line.width,
						point	=> point,
						zone	=> catch_zone)
					then
						result.append (line);
					end if;
				end if;
			end query_line;

			procedure query_net (c : in pac_nets.cursor) is
				use et_nets;
				net : type_net renames element (c);
			begin
				net.route.lines.iterate (query_line'access);
			end query_net;
			
		begin
			module.nets.iterate (query_net'access);
		end query_module;
			
	begin
		log (text => "looking up segments at" & to_string (point)
			 & " in signal layer " & to_string (layer)
			 & " catch zone" & catch_zone_to_string (catch_zone),
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
	

	procedure move_line (
		module_cursor	: in pac_generic_modules.cursor;
		line			: in type_conductor_line;
		point_of_attack	: in type_point;
		destination		: in type_point;
		log_threshold	: in type_log_level;
		net_name		: in pac_net_name.bounded_string := no_name) -- reset_n
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			net_cursor : pac_nets.cursor;

			procedure query_line (line : in out type_conductor_line) is
			begin
				-- case coordinates is
					-- when ABSOLUTE =>
						move_line_to (line, point_of_attack, destination);
						-- null;
					-- when RELATIVE =>
						-- null;
						-- CS
				-- end case;
			end query_line;

			procedure query_net (c : in pac_nets.cursor) is
				use et_nets;
				net : type_net renames element (c);
				line_cursor : pac_conductor_lines.cursor;
			begin
				line_cursor := net.route.lines.find (line);
				-- module.board.silk_screen.top.lines.update_element (line_cursor, query_line'access);
				null;
			end query_net;
			
		begin
			if net_name = no_name then
				module.nets.iterate (query_net'access);
			else
				net_cursor := module.nets.find (net_name);
				-- module.nets.update_element (net_cursor, update_net'access);
				-- CS
			end if;
		end query_module;

		
	begin
		log (text => "module " 
			& enclose_in_quotes (to_string (key (module_cursor)))
			& " signal layer" & to_string (line.layer) 
			& " moving line segment " & to_string (line)
			& " point of attack " & to_string (point_of_attack)
			& " to" & to_string (destination),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (						
			position	=> module_cursor,
			process		=> query_module'access);
		
		log_indentation_down;
	end move_line;


	
	
	procedure draw_track_arc (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in pac_net_name.bounded_string; -- reset_n
		arc				: in type_conductor_arc;
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		use pac_conductor_arcs;

		
		procedure add_freetrack (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is begin
			append (
				container	=> module.board.conductors.arcs,
				new_item	=> arc);
		end;

		
		procedure add_named_track (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			-- A track belonging to a net requires the net to be located in the given module:
			net_cursor : pac_nets.cursor := find (module.nets, net_name);

			use et_nets;
			
			procedure add (
			-- Appends the track to the net.
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net) 
			is begin
				append (
					container	=> net.route.arcs,
					new_item	=> arc);
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
		end add_named_track;

		
	begin -- draw_track_arc
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
	end draw_track_arc;

		
	
	procedure ripup_track_segment (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in pac_net_name.bounded_string; -- reset_n
		layer			: in et_pcb_stack.type_signal_layer;
		point			: in type_point; -- x/y
		accuracy		: in type_catch_zone;
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		use pac_conductor_lines;
		use pac_conductor_arcs;

		deleted : boolean := false; -- goes true if at least one segment has been ripup

		
		procedure ripup_freetrack (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			line_cursor : pac_conductor_lines.cursor := module.board.conductors.lines.first;
			arc_cursor  : pac_conductor_arcs.cursor := module.board.conductors.arcs.first;
		begin
			-- first probe the lines. If a matching line found, delete it 
			-- and abort iteration.
			while line_cursor /= pac_conductor_lines.no_element loop

				if on_segment (point, layer, line_cursor) then
					delete (module.board.conductors.lines, line_cursor);
					deleted := true;
					exit;
				end if;

				next (line_cursor);
			end loop;

			-- probe arcs if no line found.
			-- If a matching arc found, delete it and abort iteration.
			if not deleted then
				while arc_cursor /= pac_conductor_arcs.no_element loop

					if on_segment (point, layer, arc_cursor) then
						delete (module.board.conductors.arcs, arc_cursor);
						deleted := true;
						exit;
					end if;
					
					next (arc_cursor);
				end loop;
			end if;

			-- if no line and no arc found, issue warning:
			if not deleted then
				no_net_segment_found (layer, point, accuracy);
			end if;
			
		end ripup_freetrack;

		
		procedure ripup_named_track (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			-- A net belonging to a net requires the net to be located in the given module:
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

					if on_segment (point, layer, line_cursor) then
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

						if on_segment (point, layer, arc_cursor) then
							delete (net.route.arcs, arc_cursor);
							deleted := true;
							exit;
						end if;
						
						next (arc_cursor);
					end loop;
				end if;

				-- if no line and no arc found, issue warning:
				if not deleted then
					no_net_segment_found (layer, point, accuracy);
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

		
	begin -- ripup_track_segment
		log (text => "module " & to_string (module_name) &
			freetrack (net_name) &
			" ripping up segment" &
			" in layer " & to_string (layer) &
			" at" & to_string (point) &
			" accuracy" & catch_zone_to_string (accuracy),
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
		
	end ripup_track_segment;


	

	procedure place_fill_zone (
		module_cursor	: in pac_generic_modules.cursor;
		zone			: in type_zone'class;
		log_threshold	: in type_log_level;
		net_name		: in pac_net_name.bounded_string := no_name)
	is
		use ada.tags;
		use et_nets;
		
		
		procedure floating_solid (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			use pac_floating_solid;

			p : type_floating_solid := 
				type_floating_solid (zone);
			
		begin
			log (text => to_string (p, p.properties),
				level => log_threshold + 1);

			module.board.conductors.fill_zones.solid.append (p);
		end floating_solid;

		
		procedure floating_hatched (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			use pac_floating_hatched;

			p : type_floating_hatched := 
				type_floating_hatched (zone);
			
		begin
			log (text => to_string (p, p.properties),
				level => log_threshold + 1);

			module.board.conductors.fill_zones.hatched.append (p);
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
			module		: in out type_module) 
		is
			use pac_route_solid;

			p : type_route_solid := 
				type_route_solid (zone);


			procedure add_polygon (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is begin
				net.route.fill_zones.solid.append (p);
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
			module		: in out type_module) 
		is
			use pac_route_hatched;

			p : type_route_hatched := 
				type_route_hatched (zone);
			
			procedure add_polygon (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is begin
				net.route.fill_zones.hatched.append (p);
			end add_polygon;

		begin -- route_hatched
			log (text => to_string (p, p.properties, net_name),
				level => log_threshold + 1);

			update_element (
				container	=> module.nets,
				position	=> net_cursor,
				process		=> add_polygon'access);

		end route_hatched;

		
	begin -- place_fill_zone
		log (text => "module " 
			& enclose_in_quotes (to_string (key (module_cursor)))
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
	end place_fill_zone;


	procedure fill_zones (
		module_cursor	: in pac_generic_modules.cursor;
		log_category	: in type_log_category;
		log_threshold	: in type_log_level;
		nets 			: in pac_net_names.list := no_net_names)
	is separate;
	
	
	
	procedure place_text_in_conductor_layer (
		module_cursor	: in pac_generic_modules.cursor;
		signal_layer	: in type_signal_layer;
		text			: in type_text_fab_with_content;
		log_threshold	: in type_log_level)
	is
		procedure place_text (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			use pac_conductor_texts;
			vectors : pac_character_lines.list;
			use et_text;
			mirror : type_vector_text_mirrored;

			v_text : type_vector_text;
			c_text : type_conductor_text;
			
		begin
			mirror := signal_layer_to_mirror (signal_layer, deepest_conductor_layer (module_cursor));

			if mirror = YES then
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
			
			append (module.board.conductors.texts, c_text);
		end place_text;

	begin
		log (text => "module " 
			& enclose_in_quotes (to_string (key (module_cursor)))
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
	end place_text_in_conductor_layer;

	
	

	function get_texts (
		module_cursor	: in pac_generic_modules.cursor;
		point			: in type_point;
		catch_zone		: in type_catch_zone; -- the circular area around the place
		log_threshold	: in type_log_level)
		return pac_conductor_texts.list
	is
		use et_text;
		use pac_conductor_texts;
		result : pac_conductor_texts.list;

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_module) 
		is
			procedure query_text (c : in pac_conductor_texts.cursor) is
				text : type_conductor_text renames element (c);
			begin
				if in_catch_zone (
					point_1		=> point,
					catch_zone	=> catch_zone,
					point_2		=> get_place (text))
				then
					log (text => to_string (get_place (text)) 
						& " content " & enclose_in_quotes (to_string (text.content)),
						level => log_threshold + 2);
						
					result.append (text);
				end if;
			end query_text;
			
		begin
			module.board.conductors.texts.iterate (query_text'access);
		end query_module;

		
	begin
		log (text => "module " 
			& enclose_in_quotes (to_string (key (module_cursor)))
			& " looking up conductor texts at" & to_string (point) 
			& " catch zone" & catch_zone_to_string (catch_zone),
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
		point			: in type_point;
		log_threshold	: in type_log_level)
	is
		old_position : constant type_point := get_place (text);
		new_position : type_point;
		offset : type_distance_relative;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module)
		is
			text_cursor : pac_conductor_texts.cursor;

			procedure query_text (text : in out type_conductor_text) is begin
				move_text (text, offset); -- incl. vector text
			end query_text;
			
		begin
			text_cursor := module.board.conductors.texts.find (text);
			module.board.conductors.texts.update_element (text_cursor, query_text'access);
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
		
		log (text => "module " 
			& enclose_in_quotes (to_string (key (module_cursor)))
			& " moving conductor text from" & to_string (old_position)
			& " to" & to_string (new_position), -- CS by offset, signal layer number
			level => log_threshold);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);

	end move_text;

	

end et_board_ops.conductors;
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
