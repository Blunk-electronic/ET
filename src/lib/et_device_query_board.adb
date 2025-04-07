------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                   DEVICE QUERY OPERATIONS IN BOARD                       --
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

--   For correct displaying set tab width in your editor to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with et_device_appearance;				use et_device_appearance;
with et_device_purpose;					use et_device_purpose;
with et_mirroring;						use et_mirroring;
with et_schematic_ops;
with et_contour_to_polygon;
with et_logging;						use et_logging;
with et_string_processing;				use et_string_processing;
with et_package_names;					use et_package_names;
with et_device_value;					use et_device_value;
with et_device_name;					use et_device_name;


package body et_device_query_board is

	use et_symbols;
	use pac_geometry_2;



	function get_package_model (
		device_cursor	: in pac_devices_non_electric.cursor)
		return pac_package_models.cursor
	is 
		result : pac_package_models.cursor;
	begin
		result := get_package_model (element (device_cursor).package_model);
		return result;
	end get_package_model;

	
	
	function get_position (
		device_cursor	: in et_schematic.pac_devices_sch.cursor) -- IC45
		return type_package_position
	is begin
		return element (device_cursor).position;
	end get_position;
	

	function get_position (
		device_cursor	: in pac_devices_non_electric.cursor) -- FD1
		return type_package_position
	is begin
		return element (device_cursor).position;
	end get_position;


	
	function get_face (
		device_cursor	: in et_schematic.pac_devices_sch.cursor) -- IC45
		return type_face
	is 
		position : type_package_position;
	begin
		position := element (device_cursor).position;
		return get_face (position);
	end get_face;


	function get_face (
		device_cursor	: in et_pcb.pac_devices_non_electric.cursor)
		return type_face
	is 
		position : type_package_position;
	begin
		position := element (device_cursor).position;
		return get_face (position);
	end get_face;
	


	procedure terminal_not_found (
		terminal_name : in et_terminals.pac_terminal_name.bounded_string) 
	is 
		use et_terminals;
	begin
		log (ERROR,	"terminal " & enclose_in_quotes (to_string (terminal_name)) & " not found !",
			 console => true);
		raise constraint_error;
	end terminal_not_found;

	

	function get_terminal_position (
		module_cursor	: in pac_generic_modules.cursor;
		device_cursor	: in pac_devices_sch.cursor; -- IC45
		terminal_name	: in pac_terminal_name.bounded_string) -- H7, 14
		return type_terminal_position_fine
	is
		-- This is the position of the package as it is in the layout:
		package_position : et_pcb_coordinates_2.type_package_position; -- incl. angle and face

		use pac_geometry_brd;
		terminal_position : type_vector; -- x/y
		terminal_rotation : type_angle;
		terminal_position_face : type_face := TOP; -- top/bottom

		model : pac_package_model_file_name.bounded_string; -- libraries/packages/smd/SOT23.pac
		package_model_cursor : pac_package_models.cursor;

		use pac_terminals;
		-- This cursor points to the terminal in the package model:
		terminal_cursor : pac_terminals.cursor;
		
		terminal_technology : type_assembly_technology;
		
	begin
		-- Get the package model of the given device:
		model := get_package_model (device_cursor);

		-- Get the position of the package as it is in the layout:
		package_position := pac_devices_sch.element (device_cursor).position;
		
		-- Set the cursor to package model:
		package_model_cursor := get_package_model (model);

		-- Locate the desired terminal in the package model:
		terminal_cursor := get_terminal (package_model_cursor, terminal_name);
		if terminal_cursor = pac_terminals.no_element then
			terminal_not_found (terminal_name);
		end if;

		-- Get the assembly technology of the terminal (SMT or THT):
		terminal_technology := element (terminal_cursor).technology;

		-- Get x/y of the terminal as given by the package model.
		-- This position is relative to the origin of the package model:
		terminal_position := to_vector (pac_terminals.element (terminal_cursor).position.place);
		
		-- Get the rotation of the terminal (about its center) as given by the package model:
		terminal_rotation := to_angle (pac_terminals.element (terminal_cursor).position.rotation);

		-- Add to the terminal rotation the rotation of the package:
		terminal_rotation := terminal_rotation + to_angle (get_rotation (package_position));

		
		-- In the board: If the package has been flipped (to any side) by the operator
		-- then the terminal must be flipped also.
		-- If the package has not been flipped, then we assume the face of the terminal 
		-- is the same as the face of the package.
		if element (device_cursor).flipped = YES then

			case terminal_technology is
				when SMT =>
					if element (terminal_cursor).face = TOP then
						terminal_position_face := BOTTOM;
					else
						terminal_position_face := TOP;
					end if;

				when THT => 
					-- If package flipped, then the face of the THT
					-- terminal is bottom. If package not flipped, then default TOP applies:
					terminal_position_face := BOTTOM;
			end case;

			
			-- mirror terminal position alog Y axis (swap right x with left x)
			mirror (terminal_position, MIRROR_ALONG_Y_AXIS);

			-- Rotate the terminal position (x/y) by the rotation of the package:
			rotate_by (terminal_position, - terminal_rotation);
			
		else -- not flipped
			terminal_position_face := get_face (package_position);

			-- Rotate the terminal position (x/y) by the rotation of the package:
			rotate_by (terminal_position, terminal_rotation);
		end if;


		-- Move the terminal position by the position of the package:
		move_by (terminal_position, to_offset (package_position.place));

		return (
			technology	=> terminal_technology,
			place		=> terminal_position,
			rotation	=> terminal_rotation,	   
			face		=> terminal_position_face);
		
	end get_terminal_position;



	function get_all_terminals (
		device_cursor	: in et_schematic.pac_devices_sch.cursor) -- IC45
		return pac_terminals.map
	is
		use pac_package_models;
		package_model : constant pac_package_models.cursor := 
			get_package_model (device_cursor);
	begin
		return element (package_model).terminals;
	end get_all_terminals;



	function get_unconnected_terminals (
		module_cursor	: in pac_generic_modules.cursor;
		device_cursor	: in et_schematic.pac_devices_sch.cursor) -- IC45
		return pac_terminals.map
	is
		use pac_generic_modules;
		use et_nets;
		use pac_terminals;
		use et_schematic_ops;

		-- Get all terminals of the given device (according to its package variant).
		-- Later the connected terminals will be removed from this list:
		all_terminals : pac_terminals.map := get_all_terminals (device_cursor);

		-- Here we will store the terminals of the given device which are
		-- connected with nets:
		connected_terminals : pac_terminal_names.list;

		
		procedure query_net (net_cursor : in pac_nets.cursor) is
			-- Get the ports of all devices connected with the given net.
			-- Since this query is about the default assembly variant,
			-- we do not pass a specific assembly variant here.
			ports : constant type_ports := get_ports (net_cursor);

			use pac_device_ports;

			
			procedure query_device_port (d : in pac_device_ports.cursor) is

				port : type_device_port renames element (d);
				-- Now port contains the device name, unit name and port name.
				
				-- Get the cursor to the device in the schematic:
				device_cursor : constant pac_devices_sch.cursor := 
					locate_device (module_cursor, port.device_name);

				-- Get the cursor to the physical terminal (in the package model)
				-- that is linked with the port:
				terminal_cursor : constant pac_terminals.cursor := 
					get_terminal (device_cursor, port.unit_name, port.port_name);

				-- Get the terminal name (like 3 or H5):
				terminal_name : constant pac_terminal_name.bounded_string := 
					key (terminal_cursor);
				
			begin
				--put_line ("dev " & to_string (key (net_cursor)));
				if key (device_cursor) = key (get_unconnected_terminals.device_cursor) then
				
				-- Store the terminal name in list connected_terminals:
					connected_terminals.append (terminal_name);
				end if;
			end query_device_port;

			
		begin
			--put_line ("net " & to_string (key (net_cursor)));
			
			-- In variable "ports" we are interested in selector "devices" exclusively.
			-- Submodule ports and netchangers are just virtual devices
			-- that connect two conductor tracks. They can therefore be ignored:
			ports.devices.iterate (query_device_port'access);
		end query_net;

		
	begin
		--put_line ("device " & to_string (key (device_cursor)));
		--put_line ("all " & count_type'image (all_terminals.length));
		
		element (module_cursor).nets.iterate (query_net'access);

		--put_line ("connected " & count_type'image (connected_terminals.length));
		
		-- Remove the connected_terminals from all_terminals
		-- so that only the unconneced terminals are left:
		remove_terminals (all_terminals, connected_terminals);

		return all_terminals;
	end get_unconnected_terminals;


	

	function to_polygon (
		module_cursor	: in pac_generic_modules.cursor;
		device_cursor	: in pac_devices_sch.cursor;
		terminal_cursor	: in pac_terminals.cursor;
		layer_category	: in type_signal_layer_category;
		tolerance		: in type_distance_positive)
		return type_terminal_polygon
	is
		use pac_polygons;
		exists : boolean := false;
		result : type_polygon; -- to be returned

		use pac_contours;
		use pac_terminals;
		
		-- Get the actual terminal as described in the package model:
		terminal : constant et_terminals.type_terminal := element (terminal_cursor);
		
		-- Get the terminal name (like 3 or H5):
		terminal_name : constant pac_terminal_name.bounded_string := key (terminal_cursor);
		
		-- Get the terminal position (incl. rotation and face):
		terminal_position : constant type_terminal_position_fine := 
			get_terminal_position (module_cursor, device_cursor, terminal_name);

		-- The displacement required to move the contour to 
		-- its final position:
		terminal_displacement : constant type_distance_relative := 
			to_distance_relative (terminal_position.place);

		
		-- intermediate storage place of a contour:
		contour : type_contour;


		-- Converts the contour to a polygon:
		procedure make_polygon is 
			use et_contour_to_polygon;
		begin
			exists := true;
			
			result := to_polygon (
				contour		=> contour,
				tolerance	=> tolerance,
				mode		=> EXPAND, -- CS ?
				debug		=> false);
		end make_polygon;


		-- Mirrors the contour (if terminal is flipped to bottom side) and
		-- rotates the contour:
		procedure mirror_and_rotate is begin
			if terminal_position.face = BOTTOM then
				mirror (contour, MIRROR_ALONG_Y_AXIS);

				-- if on bottom side: rotate CW
				rotate_by (contour, - to_rotation (terminal_position.rotation));
			else
				-- if on top side: rotate CCW
				rotate_by (contour, + to_rotation (terminal_position.rotation));
			end if;
		end mirror_and_rotate;


		-- Moves the contour to the final position and converts it to a polygon.
		-- Optionally, if required by the caller, offsets the polygon edges
		-- by the width of the inner signal layer:
		procedure finalize (do_offset : in boolean := false) is
			use et_board_shapes_and_text.pac_polygon_offsetting;
		begin
			move_by (contour, terminal_displacement);
			make_polygon;
			if do_offset then
				offset_polygon (result, type_float (terminal.width_inner_layers));
			end if;
		end finalize;
				
		
	begin -- to_polygon

		case terminal.technology is
			when THT => 
				case layer_category is
					when INNER =>								
						case terminal.tht_hole is
							when DRILLED =>
								contour := get_inner_contour (terminal, terminal_position.place);
								make_polygon;										
								
							when MILLED =>
								contour := terminal.millings;
								mirror_and_rotate;										
								finalize (do_offset => true);
								
						end case;
					
					when OUTER_TOP =>
						if terminal_position.face = TOP then
							contour := terminal.pad_shape_tht.top;
						else
							contour := terminal.pad_shape_tht.bottom;
						end if;
						mirror_and_rotate;
						finalize;

					when OUTER_BOTTOM =>
						if terminal_position.face = BOTTOM then
							contour := terminal.pad_shape_tht.top;
						else
							contour := terminal.pad_shape_tht.bottom;
						end if;
						mirror_and_rotate;
						finalize;
				end case;
				

			when SMT =>
				if layer_category = OUTER_TOP and terminal_position.face = TOP then
					contour := terminal.pad_shape_smt;
					rotate_by (contour, to_rotation (terminal_position.rotation));
					finalize;						
					
				elsif layer_category = OUTER_BOTTOM and terminal_position.face = BOTTOM then
					contour := terminal.pad_shape_smt;
					mirror (contour, MIRROR_ALONG_Y_AXIS);
					rotate_by (contour, - to_rotation (terminal_position.rotation));
					finalize;
				end if;
		end case;


		if exists then
			return (
				exists		=> TRUE, 
				polygon		=> result, 
				position	=> terminal_position);
		else
			return (exists => FALSE);
		end if;

	end to_polygon;



	
-- CONDUCTORS
	

	function get_conductor_objects (
		device_cursor	: in pac_devices_sch.cursor;
		layer_category	: in type_signal_layer_category)
		return type_conductor_objects
	is
		conductors : type_conductor_objects; -- to be returned
		device : type_device_sch renames element (device_cursor);
		packge : pac_package_models.cursor;
	begin
		if device.appearance = APPEARANCE_PCB then
			packge := get_package_model (device_cursor);

			if layer_category /= INNER then -- non-electric conductor objects exist in outer layers only
				if device.flipped = NO then
					conductors := get_conductor_objects (packge, layer_category);
					rotate_conductor_objects (conductors, + device.position.rotation);
				else
					conductors := get_conductor_objects (packge, invert_category (layer_category));
					mirror_conductor_objects (conductors);
					rotate_conductor_objects (conductors, - device.position.rotation);
				end if;

				move_conductor_objects (conductors, to_distance_relative (device.position.place));
			end if;
		end if;
		
		return conductors;
	end get_conductor_objects;
	

	
	function get_conductor_polygons (
		device_cursor	: in pac_devices_sch.cursor;
		layer_category	: in type_signal_layer_category)
		return pac_polygon_list.list
	is
		result : pac_polygon_list.list;
		use et_contour_to_polygon;
		
		device : type_device_sch renames element (device_cursor);
		packge : pac_package_models.cursor;
		conductors : type_conductor_objects; -- non-electrical
	begin
		if device.appearance = APPEARANCE_PCB then
			packge := get_package_model (device_cursor);

			if layer_category /= INNER then -- non-electric conductor objects exist in outer layers only
				if device.flipped = NO then
					conductors := get_conductor_objects (packge, layer_category);
					rotate_conductor_objects (conductors, + device.position.rotation);
				else
					conductors := get_conductor_objects (packge, invert_category (layer_category));
					mirror_conductor_objects (conductors);
					rotate_conductor_objects (conductors, - device.position.rotation);
				end if;

				move_conductor_objects (conductors, to_distance_relative (device.position.place));

				-- convert conductor objects to polygons:
				result := to_polygons (conductors, fill_tolerance);
			end if;
		end if;
		
		return result;
	end get_conductor_polygons;

	
	
	function get_conductor_objects (
		device_cursor	: in pac_devices_non_electric.cursor;
		layer_category	: in type_signal_layer_category)
		return type_conductor_objects
	is
		conductors : type_conductor_objects; -- to be returned
		
		device : type_device_non_electric renames element (device_cursor);
		packge : constant pac_package_models.cursor := get_package_model (device.package_model);
		
		offset : constant type_distance_relative := to_distance_relative (device.position.place);
	begin
		-- lines, arcs, circles, texts
		if layer_category /= INNER then -- non-electric conductor objects exist in outer layers only
			if device.flipped = NO then
				conductors := get_conductor_objects (packge, layer_category);
				rotate_conductor_objects (conductors, + device.position.rotation);
			else
				conductors := get_conductor_objects (packge, invert_category (layer_category));
				mirror_conductor_objects (conductors);
				rotate_conductor_objects (conductors, - device.position.rotation);
			end if;

			move_conductor_objects (conductors, offset);
		end if;

		return conductors;
	end get_conductor_objects;



	function get_conductor_polygons (
		device_cursor	: in pac_devices_non_electric.cursor;
		layer_category	: in type_signal_layer_category)
		return pac_polygon_list.list
	is
		result : pac_polygon_list.list;
		
		device : type_device_non_electric renames element (device_cursor);
		packge : constant pac_package_models.cursor := get_package_model (device.package_model);
		
		offset : constant type_distance_relative := to_distance_relative (device.position.place);

		use pac_contours;
		terminals	: pac_contour_list.list;
		
		conductors	: type_conductor_objects; -- non-electrical
		conductor_polygons : pac_polygon_list.list;

		use et_contour_to_polygon;
	begin
		-- TERMINALS:
		if device.flipped = NO then
			terminals := get_terminal_contours (packge, layer_category);
			rotate_contours (terminals, device.position.rotation);
		else
			terminals := get_terminal_contours (packge, invert_category (layer_category));
			mirror_contours (terminals);
			rotate_contours (terminals, - device.position.rotation);
		end if;

		move_contours (terminals, offset);
		
		result := to_polygons (
			contours	=> terminals,
			tolerance	=> fill_tolerance,
			mode		=> EXPAND,
			debug		=> false);


		-- CONDUCTOR OBJECTS (lines, arcs, circles, texts)
		if layer_category /= INNER then -- non-electric conductor objects exist in outer layers only
			if device.flipped = NO then
				conductors := get_conductor_objects (packge, layer_category);
				rotate_conductor_objects (conductors, + device.position.rotation);
			else
				conductors := get_conductor_objects (packge, invert_category (layer_category));
				mirror_conductor_objects (conductors);
				rotate_conductor_objects (conductors, - device.position.rotation);
			end if;

			move_conductor_objects (conductors, offset);

			-- convert conductor objects to polygons:
			conductor_polygons := to_polygons (conductors, fill_tolerance);
		end if;
		
		result.splice (before => pac_polygon_list.no_element, source => conductor_polygons);

		return result;
	end get_conductor_polygons;
	


	
-- ROUTE RESTRICT
	
	function get_route_restrict_objects (
		device_cursor	: in pac_devices_sch.cursor;
		layer_category	: in type_signal_layer_category)
		return et_route_restrict.packages.type_one_side
	is	
		use et_route_restrict.packages;
		restrict : type_one_side; -- to be returned
		device : type_device_sch renames element (device_cursor);
		packge : pac_package_models.cursor;
	begin
		if device.appearance = APPEARANCE_PCB then
			packge := get_package_model (device_cursor);
				
			if layer_category /= INNER then -- route restrict objects exist in outer layers only
				if device.flipped = NO then
					restrict := get_route_restrict_objects (packge, layer_category);
					rotate_route_restrict_objects (restrict, + device.position.rotation);
				else
					restrict := get_route_restrict_objects (packge, invert_category (layer_category));
					mirror_route_restrict_objects (restrict);
					rotate_route_restrict_objects (restrict, - device.position.rotation);
				end if;

				move_route_restrict_objects (restrict, to_distance_relative (device.position.place));
			end if;
		end if;

		return restrict;
	end get_route_restrict_objects;

	

	function get_route_restrict_polygons (
		device_cursor	: in pac_devices_sch.cursor;
		layer_category	: in type_signal_layer_category)
		return pac_polygon_list.list
	is
		result : pac_polygon_list.list;
		device : type_device_sch renames element (device_cursor);
		packge : pac_package_models.cursor;

		use et_route_restrict.packages;
		restrict : type_one_side;
	begin
		if device.appearance = APPEARANCE_PCB then
			packge := get_package_model (device_cursor);
				
			if layer_category /= INNER then -- route restrict objects exist in outer layers only
				if device.flipped = NO then
					restrict := get_route_restrict_objects (packge, layer_category);
					rotate_route_restrict_objects (restrict, + device.position.rotation);
				else
					restrict := get_route_restrict_objects (packge, invert_category (layer_category));
					mirror_route_restrict_objects (restrict);
					rotate_route_restrict_objects (restrict, - device.position.rotation);
				end if;

				move_route_restrict_objects (restrict, to_distance_relative (device.position.place));

				-- convert restrict objects to polygons:
				result := to_polygons (restrict, fill_tolerance);
			end if;
		end if;
		
		return result;
	end get_route_restrict_polygons;
	


	function get_route_restrict_objects (
		device_cursor	: in pac_devices_non_electric.cursor;
		layer_category	: in type_signal_layer_category)
		return et_route_restrict.packages.type_one_side
	is
		use et_route_restrict.packages;
		restrict : type_one_side; -- to be returned
		device : type_device_non_electric renames element (device_cursor);
		packge : constant pac_package_models.cursor := get_package_model (device.package_model);

		rotation : type_rotation_model renames device.position.rotation;
	begin
		if layer_category /= INNER then -- route restrict objects exist in outer layers only
			if device.flipped = NO then
				restrict := get_route_restrict_objects (packge, layer_category);
				rotate_route_restrict_objects (restrict, + device.position.rotation);
			else
				restrict := get_route_restrict_objects (packge, invert_category (layer_category));
				mirror_route_restrict_objects (restrict);
				rotate_route_restrict_objects (restrict, - device.position.rotation);
			end if;

			move_route_restrict_objects (restrict, to_distance_relative (device.position.place));
		end if;

		return restrict;
	end get_route_restrict_objects;


	
	function get_route_restrict_polygons (
		device_cursor	: in pac_devices_non_electric.cursor;
		layer_category	: in type_signal_layer_category)
		return pac_polygon_list.list
	is
		result : pac_polygon_list.list;
		
		device : type_device_non_electric renames element (device_cursor);
		packge : constant pac_package_models.cursor := get_package_model (device.package_model);
		
		use et_route_restrict.packages;
		restrict : type_one_side;
	begin
		if layer_category /= INNER then -- route restrict objects exist in outer layers only
			if device.flipped = NO then
				restrict := get_route_restrict_objects (packge, layer_category);
				rotate_route_restrict_objects (restrict, + device.position.rotation);
			else
				restrict := get_route_restrict_objects (packge, invert_category (layer_category));
				mirror_route_restrict_objects (restrict);
				rotate_route_restrict_objects (restrict, - device.position.rotation);
			end if;

			move_route_restrict_objects (restrict, to_distance_relative (device.position.place));

			-- convert restrict objects to polygons:
			result := to_polygons (restrict, fill_tolerance);
		end if;

		return result;
	end get_route_restrict_polygons;
	

	
	
-- VIA RESTRICT
	
	function get_via_restrict_objects (
		device_cursor	: in pac_devices_sch.cursor;
		layer_category	: in type_signal_layer_category)
		return et_via_restrict.packages.type_one_side
	is		
		use et_via_restrict.packages;
		restrict : type_one_side; -- to be returned
		
		device : type_device_sch renames element (device_cursor);
		packge : pac_package_models.cursor;
	begin
		if device.appearance = APPEARANCE_PCB then
			packge := get_package_model (device_cursor);
				
			if layer_category /= INNER then -- via restrict objects exist in outer layers only
				if device.flipped = NO then
					restrict := get_via_restrict_objects (packge, layer_category);
					rotate_via_restrict_objects (restrict, + device.position.rotation);
				else
					restrict := get_via_restrict_objects (packge, invert_category (layer_category));
					mirror_via_restrict_objects (restrict);
					rotate_via_restrict_objects (restrict, - device.position.rotation);
				end if;

				move_via_restrict_objects (restrict, to_distance_relative (device.position.place));
			end if;
		end if;

		return restrict;
	end get_via_restrict_objects;



	function get_via_restrict_objects (
		device_cursor	: in pac_devices_non_electric.cursor;
		layer_category	: in type_signal_layer_category)
		return et_via_restrict.packages.type_one_side
	is
		use et_via_restrict.packages;		
		restrict : type_one_side; -- to be returned
		
		device : type_device_non_electric renames element (device_cursor);
		packge : constant pac_package_models.cursor := get_package_model (device.package_model);

		rotation : type_rotation_model renames device.position.rotation;
	begin
		if layer_category /= INNER then -- via restrict objects exist in outer layers only
			if device.flipped = NO then
				restrict := get_via_restrict_objects (packge, layer_category);
				rotate_via_restrict_objects (restrict, + device.position.rotation);
			else
				restrict := get_via_restrict_objects (packge, invert_category (layer_category));
				mirror_via_restrict_objects (restrict);
				rotate_via_restrict_objects (restrict, - device.position.rotation);
			end if;

			move_via_restrict_objects (restrict, to_distance_relative (device.position.place));
		end if;

		return restrict;
	end get_via_restrict_objects;
	


	
-- KEEPOUT
	
	function get_keepout_objects (
		device_cursor	: in pac_devices_sch.cursor;
		face			: in type_face)
		return type_keepout
	is
		result : type_keepout;
		device : type_device_sch renames element (device_cursor);
		packge : pac_package_models.cursor;
		rotation : type_rotation_model;
	begin
		if device.appearance = APPEARANCE_PCB then
			packge := get_package_model (device_cursor);
			rotation := device.position.rotation;
			
			case face is
				when TOP =>
					if device.flipped = NO then
						result := get_keepout_objects (packge, TOP);
						rotate_keepout_objects (result, + rotation);
					else
						result := get_keepout_objects (packge, BOTTOM);
						mirror_keepout_objects (result);
						rotate_keepout_objects (result, - rotation);
					end if;

				when BOTTOM =>
					if device.flipped = NO then
						result := get_keepout_objects (packge, BOTTOM);
						rotate_keepout_objects (result, + rotation);
					else
						result := get_keepout_objects (packge, TOP);
						mirror_keepout_objects (result);
						rotate_keepout_objects (result, - rotation);
					end if;
			end case;
		end if;
		
		move_keepout_objects (result, to_distance_relative (device.position.place));
		return result;
	end get_keepout_objects;


	
	function get_keepout_objects (
		device_cursor	: in pac_devices_non_electric.cursor;
		face			: in type_face)
		return type_keepout
	is
		result : type_keepout;

		device : type_device_non_electric renames element (device_cursor);
		packge : constant pac_package_models.cursor := get_package_model (device.package_model);

		rotation : type_rotation_model renames device.position.rotation;
	begin
		case face is
			when TOP =>
				if device.flipped = NO then
					result := get_keepout_objects (packge, TOP);
					rotate_keepout_objects (result, + rotation);
				else
					result := get_keepout_objects (packge, BOTTOM);
					mirror_keepout_objects (result);
					rotate_keepout_objects (result, - rotation);
				end if;

			when BOTTOM =>
				if device.flipped = NO then
					result := get_keepout_objects (packge, BOTTOM);
					rotate_keepout_objects (result, + rotation);
				else
					result := get_keepout_objects (packge, TOP);
					mirror_keepout_objects (result);
					rotate_keepout_objects (result, - rotation);
				end if;
		end case;

		move_keepout_objects (result, to_distance_relative (device.position.place));
		
		return result;
	end get_keepout_objects;


-- STENCIL
	
	function get_stencil_objects (
		device_cursor	: in pac_devices_sch.cursor;
		face			: in type_face)
		return type_stencil
	is
		result : type_stencil;
		device : type_device_sch renames element (device_cursor);
		packge : pac_package_models.cursor;
		rotation : type_rotation_model;
	begin
		if device.appearance = APPEARANCE_PCB then
			packge := get_package_model (device_cursor);
			rotation := device.position.rotation;
			
			case face is
				when TOP =>
					if device.flipped = NO then
						result := get_stencil_objects (packge, TOP);
						rotate_stencil_objects (result, + rotation);
					else
						result := get_stencil_objects (packge, BOTTOM);
						mirror_stencil_objects (result);
						rotate_stencil_objects (result, - rotation);
					end if;

				when BOTTOM =>
					if device.flipped = NO then
						result := get_stencil_objects (packge, BOTTOM);
						rotate_stencil_objects (result, + rotation);
					else
						result := get_stencil_objects (packge, TOP);
						mirror_stencil_objects (result);
						rotate_stencil_objects (result, - rotation);
					end if;
			end case;
		end if;
		
		move_stencil_objects (result, to_distance_relative (device.position.place));
		return result;
	end get_stencil_objects;



	function get_stencil_objects (
		device_cursor	: in pac_devices_non_electric.cursor;
		face			: in type_face)
		return type_stencil
	is
		result : type_stencil;

		device : type_device_non_electric renames element (device_cursor);
		packge : constant pac_package_models.cursor := get_package_model (device.package_model);

		rotation : type_rotation_model renames device.position.rotation;
	begin
		case face is
			when TOP =>
				if device.flipped = NO then
					result := get_stencil_objects (packge, TOP);
					rotate_stencil_objects (result, + rotation);
				else
					result := get_stencil_objects (packge, BOTTOM);
					mirror_stencil_objects (result);
					rotate_stencil_objects (result, - rotation);
				end if;

			when BOTTOM =>
				if device.flipped = NO then
					result := get_stencil_objects (packge, BOTTOM);
					rotate_stencil_objects (result, + rotation);
				else
					result := get_stencil_objects (packge, TOP);
					mirror_stencil_objects (result);
					rotate_stencil_objects (result, - rotation);
				end if;
		end case;
		
		move_stencil_objects (result, to_distance_relative (device.position.place));
		return result;
	end get_stencil_objects;



-- STOPMASK:

	function get_stopmask_objects (
		device_cursor	: in pac_devices_sch.cursor;
		face			: in type_face)
		return type_stopmask
	is
		result : type_stopmask;
		device : type_device_sch renames element (device_cursor);
		packge : pac_package_models.cursor;
		rotation : type_rotation_model;

		use et_stopmask.packages;
	begin
		if device.appearance = APPEARANCE_PCB then
			packge := get_package_model (device_cursor);
			rotation := device.position.rotation;

			case face is
				when TOP =>
					if device.flipped = NO then
						result := get_stopmask_objects (packge, TOP);
						rotate_stopmask_objects (result, + rotation);
					else
						result := get_stopmask_objects (packge, BOTTOM);
						mirror_stopmask_objects (result);
						rotate_stopmask_objects (result, - rotation);
					end if;

				when BOTTOM =>
					if device.flipped = NO then
						result := get_stopmask_objects (packge, BOTTOM);
						rotate_stopmask_objects (result, + rotation);
					else
						result := get_stopmask_objects (packge, TOP);
						mirror_stopmask_objects (result);
						rotate_stopmask_objects (result, - rotation);
					end if;
			end case;

			move_stopmask_objects (result, to_distance_relative (device.position.place));			
		end if;

		return result;
	end get_stopmask_objects;


	function get_stopmask_objects (
		device_cursor	: in pac_devices_non_electric.cursor;
		face			: in type_face)
		return type_stopmask
	is
		result : type_stopmask;

		device : type_device_non_electric renames element (device_cursor);
		packge : constant pac_package_models.cursor := get_package_model (device.package_model);

		rotation : type_rotation_model renames device.position.rotation;

		use et_stopmask.packages;
	begin
		case face is
			when TOP =>
				if device.flipped = NO then
					result := get_stopmask_objects (packge, TOP);
					rotate_stopmask_objects (result, + rotation);
				else
					result := get_stopmask_objects (packge, BOTTOM);
					mirror_stopmask_objects (result);
					rotate_stopmask_objects (result, - rotation);
				end if;

			when BOTTOM =>
				if device.flipped = NO then
					result := get_stopmask_objects (packge, BOTTOM);
					rotate_stopmask_objects (result, + rotation);
				else
					result := get_stopmask_objects (packge, TOP);
					mirror_stopmask_objects (result);
					rotate_stopmask_objects (result, - rotation);
				end if;
		end case;
		
		move_stopmask_objects (result, to_distance_relative (device.position.place));
		return result;
	end get_stopmask_objects;

	

-- PLACEHOLDERS
	

	function to_placeholder_content (
		device_cursor	: in pac_devices_sch.cursor;
		placeholder		: in type_placeholder)
		return et_text.pac_text_content.bounded_string 
	is
		device : type_device_sch renames element (device_cursor);

		use et_text;
		result : pac_text_content.bounded_string;
	begin
		case placeholder.meaning is
			when NAME 		=> result := to_content (to_string (key (device_cursor)));
			when VALUE		=> result := to_content (to_string (device.value));
			when PURPOSE	=> result := to_content (to_string (device.purpose));
		end case;
		
		return result;
	end to_placeholder_content;



	function to_placeholder_content (
		device_cursor	: in pac_devices_non_electric.cursor;
		placeholder		: in type_placeholder)
		return et_text.pac_text_content.bounded_string 
	is
		device : type_device_non_electric renames element (device_cursor);

		use et_text;
		result : pac_text_content.bounded_string;
	begin
		case placeholder.meaning is
			when NAME 		=> result := to_content (to_string (key (device_cursor)));
			-- CS
			--when VALUE		=> result := to_content (to_string (device.value));
			--when PURPOSE	=> result := to_content (to_string (device.purpose));
			when others => null;
		end case;
		
		return result;
	end to_placeholder_content;
	

	
-- SILKSCREEN

	function get_silkscreen_objects (
		device_cursor	: in pac_devices_sch.cursor;
		face			: in type_face)
		return type_silkscreen
	is
		result : type_silkscreen;
		device : type_device_sch renames element (device_cursor);
		packge : pac_package_models.cursor;
		rotation : type_rotation_model;

		use et_silkscreen.packages;
		silkscreen : et_silkscreen.packages.type_silkscreen_package;


		-- Converts the placeholders to a list of regular texts
		-- and appends them to the silkscreen.texts:
		procedure convert_placeholders_to_texts is
			use pac_placeholders;

			procedure query_placeholder (c : in pac_placeholders.cursor) is
				ph : type_placeholder renames element (c);
				use pac_text_board;
				text : type_silk_text := (type_text_fab (ph) with others => <>);
			begin
				text.content := to_placeholder_content (device_cursor, ph); -- map from meaning to content

				-- Ignore the text if it has no content:
				if not is_empty (text.content) then
					silkscreen.texts.append (text);
				end if;
			end query_placeholder;
			
		begin
			silkscreen.placeholders.iterate (query_placeholder'access);		
		end convert_placeholders_to_texts;

		
	begin -- get_silkscreen_objects
		if device.appearance = APPEARANCE_PCB then
			packge := get_package_model (device_cursor);
			rotation := device.position.rotation;

			case face is
				when TOP =>
					if device.flipped = NO then
						silkscreen := get_silkscreen_objects (packge, TOP);

						-- overwrite the default placeholders: -- CS see spec of this function
						silkscreen.placeholders := device.text_placeholders.silkscreen.top;
						convert_placeholders_to_texts;
						rotate_silkscreen_objects (silkscreen, + rotation);
					else
						silkscreen := get_silkscreen_objects (packge, BOTTOM);
						
						-- overwrite the default placeholders: -- CS see spec of this function
						silkscreen.placeholders := device.text_placeholders.silkscreen.bottom;
						convert_placeholders_to_texts;
						mirror_silkscreen_objects (silkscreen);
						rotate_silkscreen_objects (silkscreen, - rotation);
					end if;

				when BOTTOM =>
					if device.flipped = NO then
						silkscreen := get_silkscreen_objects (packge, BOTTOM);
						
						-- overwrite the default placeholders: -- CS see spec of this function
						silkscreen.placeholders := device.text_placeholders.silkscreen.bottom;
						convert_placeholders_to_texts;
						rotate_silkscreen_objects (silkscreen, + rotation);
					else
						silkscreen := get_silkscreen_objects (packge, TOP);
						
						-- overwrite the default placeholders: -- CS see spec of this function
						silkscreen.placeholders := device.text_placeholders.silkscreen.top;
						convert_placeholders_to_texts;
						mirror_silkscreen_objects (silkscreen);
						rotate_silkscreen_objects (silkscreen, - rotation);
					end if;
			end case;

			move_silkscreen_objects (silkscreen, to_distance_relative (device.position.place));			
		end if;

		result := type_silkscreen (silkscreen);		
		return result;
	end get_silkscreen_objects;
	

	
	function get_silkscreen_objects (
		device_cursor	: in pac_devices_non_electric.cursor;
		face			: in type_face)
		return type_silkscreen
	is
		result : type_silkscreen;		

		device : type_device_non_electric renames element (device_cursor);
		packge : constant pac_package_models.cursor := get_package_model (device.package_model);

		rotation : type_rotation_model renames device.position.rotation;

		use et_silkscreen.packages;
		silkscreen : et_silkscreen.packages.type_silkscreen_package;

		
		-- Converts the placeholders to a list of regular texts
		-- and appends them to the silkscreen.texts:
		procedure convert_placeholders_to_texts is
			use pac_placeholders;

			procedure query_placeholder (c : in pac_placeholders.cursor) is
				ph : type_placeholder renames element (c);
				use pac_text_board;
				text : type_silk_text := (type_text_fab (ph) with others => <>);
			begin
				text.content := to_placeholder_content (device_cursor, ph); -- map from meaning to content

				-- Ignore the text if it has no content:
				if not is_empty (text.content) then
					silkscreen.texts.append (text);
				end if;
			end query_placeholder;
			
		begin
			silkscreen.placeholders.iterate (query_placeholder'access);		
		end convert_placeholders_to_texts;
	
		
	begin -- get_silkscreen_objects
		case face is
			when TOP =>
				if device.flipped = NO then
					silkscreen := get_silkscreen_objects (packge, TOP);
					
					-- overwrite the default placeholders: -- CS see spec of this function
					silkscreen.placeholders := device.text_placeholders.silkscreen.top;
					convert_placeholders_to_texts;
					rotate_silkscreen_objects (silkscreen, + rotation);
				else
					silkscreen := get_silkscreen_objects (packge, BOTTOM);
					
					-- overwrite the default placeholders: -- CS see spec of this function
					silkscreen.placeholders := device.text_placeholders.silkscreen.bottom;
					convert_placeholders_to_texts;
					mirror_silkscreen_objects (silkscreen);
					rotate_silkscreen_objects (silkscreen, - rotation);
				end if;

			when BOTTOM =>
				if device.flipped = NO then
					silkscreen := get_silkscreen_objects (packge, BOTTOM);
					
					-- overwrite the default placeholders: -- CS see spec of this function
					silkscreen.placeholders := device.text_placeholders.silkscreen.bottom;
					convert_placeholders_to_texts;
					rotate_silkscreen_objects (silkscreen, + rotation);
				else
					silkscreen := get_silkscreen_objects (packge, TOP);
					
					-- overwrite the default placeholders: -- CS see spec of this function
					silkscreen.placeholders := device.text_placeholders.silkscreen.top;
					convert_placeholders_to_texts;
					mirror_silkscreen_objects (silkscreen);
					rotate_silkscreen_objects (silkscreen, - rotation);
				end if;
		end case;
		
		move_silkscreen_objects (silkscreen, to_distance_relative (device.position.place));

		result := type_silkscreen (silkscreen);
		return result;
	end get_silkscreen_objects;



	
-- ASSEMBLY DOCUMENTATION:
	
	function get_assy_doc_objects (
		device_cursor	: in pac_devices_sch.cursor;
		face			: in type_face)
		return type_assy_doc
	is
		result : type_assy_doc;
		device : type_device_sch renames element (device_cursor);
		packge : pac_package_models.cursor;
		rotation : type_rotation_model;

		use et_assy_doc.packages;
		assy_doc : et_assy_doc.packages.type_assy_doc_package;


		-- Converts the placeholders to a list of regular texts
		-- and appends them to the assy_doc.texts:
		procedure convert_placeholders_to_texts is
			use pac_placeholders;

			procedure query_placeholder (c : in pac_placeholders.cursor) is
				ph : type_placeholder renames element (c);
				use pac_text_board;
				text : type_doc_text := (type_text_fab (ph) with others => <>);
			begin
				text.content := to_placeholder_content (device_cursor, ph); -- map from meaning to content

				-- Ignore the text if it has no content:
				if not is_empty (text.content) then
					assy_doc.texts.append (text);
				end if;
			end query_placeholder;
			
		begin
			assy_doc.placeholders.iterate (query_placeholder'access);		
		end convert_placeholders_to_texts;

		
	begin -- get_assy_doc_objects
		if device.appearance = APPEARANCE_PCB then
			packge := get_package_model (device_cursor);
			rotation := device.position.rotation;

			case face is
				when TOP =>
					if device.flipped = NO then
						assy_doc := get_assy_doc_objects (packge, TOP);
						
						-- overwrite the default placeholders: -- CS see spec of this function
						assy_doc.placeholders := device.text_placeholders.assy_doc.top;
						convert_placeholders_to_texts;
						rotate_assy_doc_objects (assy_doc, + rotation);
					else
						assy_doc := get_assy_doc_objects (packge, BOTTOM);
						
						-- overwrite the default placeholders: -- CS see spec of this function
						assy_doc.placeholders := device.text_placeholders.assy_doc.bottom;
						convert_placeholders_to_texts;
						mirror_assy_doc_objects (assy_doc);
						rotate_assy_doc_objects (assy_doc, - rotation);
					end if;

				when BOTTOM =>
					if device.flipped = NO then
						assy_doc := get_assy_doc_objects (packge, BOTTOM);

						-- overwrite the default placeholders: -- CS see spec of this function
						assy_doc.placeholders := device.text_placeholders.assy_doc.bottom;
						convert_placeholders_to_texts;
						rotate_assy_doc_objects (assy_doc, + rotation);
					else
						assy_doc := get_assy_doc_objects (packge, TOP);

						-- overwrite the default placeholders: -- CS see spec of this function
						assy_doc.placeholders := device.text_placeholders.assy_doc.top;
						convert_placeholders_to_texts;
						mirror_assy_doc_objects (assy_doc);
						rotate_assy_doc_objects (assy_doc, - rotation);
					end if;
			end case;

			move_assy_doc_objects (assy_doc, to_distance_relative (device.position.place));			
		end if;

		result := type_assy_doc (assy_doc);
		return result;
	end get_assy_doc_objects;
	

	function get_assy_doc_objects (
		device_cursor	: in pac_devices_non_electric.cursor;
		face			: in type_face)
		return type_assy_doc
	is
		result : type_assy_doc;		

		device : type_device_non_electric renames element (device_cursor);
		packge : constant pac_package_models.cursor := get_package_model (device.package_model);

		rotation : type_rotation_model renames device.position.rotation;

		use et_assy_doc.packages;
		assy_doc : et_assy_doc.packages.type_assy_doc_package;


		-- Converts the placeholders to a list of regular texts
		-- and appends them to the assy_doc.texts:
		procedure convert_placeholders_to_texts is
			use pac_placeholders;

			procedure query_placeholder (c : in pac_placeholders.cursor) is
				ph : type_placeholder renames element (c);
				use pac_text_board;
				text : type_doc_text := (type_text_fab (ph) with others => <>);
			begin
				text.content := to_placeholder_content (device_cursor, ph); -- map from meaning to content

				-- Ignore the text if it has no content:
				if not is_empty (text.content) then
					
					-- Vectorize the content of the placeholder:
					-- text.vectors := vectorize_text (
					-- 	content		=> text.content,
					-- 	size		=> ph.size,
					-- 	--rotation	=> add (get_rotation (ph.position), get_rotation (package_position)),
					-- 	rotation	=> get_rotation (ph.position),
					-- 	position	=> ph.position.place,
					-- 	mirror		=> MIRROR_NO,
					-- 	line_width	=> ph.line_width,
					-- 	alignment	=> ph.alignment); -- right, bottom

					assy_doc.texts.append (text);
				end if;
			end query_placeholder;
			
		begin
			assy_doc.placeholders.iterate (query_placeholder'access);		
		end convert_placeholders_to_texts;

		
	begin -- get_assy_doc_objects
		case face is
			when TOP =>
				if device.flipped = NO then
					assy_doc := get_assy_doc_objects (packge, TOP);
					
					-- overwrite the default placeholders: -- CS see spec of this function
					assy_doc.placeholders := device.text_placeholders.assy_doc.top;
					convert_placeholders_to_texts;
					rotate_assy_doc_objects (assy_doc, + rotation);
				else
					assy_doc := get_assy_doc_objects (packge, BOTTOM);
					
					-- overwrite the default placeholders: -- CS see spec of this function
					assy_doc.placeholders := device.text_placeholders.assy_doc.bottom;
					convert_placeholders_to_texts;
					mirror_assy_doc_objects (assy_doc);
					rotate_assy_doc_objects (assy_doc, - rotation);
				end if;

			when BOTTOM =>
				if device.flipped = NO then
					assy_doc := get_assy_doc_objects (packge, BOTTOM);

					-- overwrite the default placeholders: -- CS see spec of this function
					assy_doc.placeholders := device.text_placeholders.assy_doc.bottom;
					convert_placeholders_to_texts;
					rotate_assy_doc_objects (assy_doc, + rotation);
				else
					assy_doc := get_assy_doc_objects (packge, TOP);

					-- overwrite the default placeholders: -- CS see spec of this function
					assy_doc.placeholders := device.text_placeholders.assy_doc.top;
					convert_placeholders_to_texts;
					mirror_assy_doc_objects (assy_doc);
					rotate_assy_doc_objects (assy_doc, - rotation);
				end if;
		end case;
		
		move_assy_doc_objects (assy_doc, to_distance_relative (device.position.place));

		result := type_assy_doc (assy_doc);
		return result;
	end get_assy_doc_objects;

	
	
-- HOLES
	
	function get_holes (
		device_cursor	: in pac_devices_sch.cursor)
		return pac_holes.list
	is
		holes : pac_holes.list; -- to be returned
		
		device : type_device_sch renames element (device_cursor);
		packge : pac_package_models.cursor;

		rotation : type_rotation_model;
	begin
		if device.appearance = APPEARANCE_PCB then
			packge := get_package_model (device_cursor);
			rotation := device.position.rotation;
			
			holes := get_hole_contours (packge);
					
			if device.flipped = YES then
				mirror_holes (holes);
				rotate_holes (holes, - rotation);
			else
				rotate_holes (holes, + rotation);
			end if;
		
			move_holes (holes, to_distance_relative (device.position.place));
		end if;
		
		return holes;		
	end get_holes;


	
	function get_hole_polygons (
		device_cursor	: in pac_devices_sch.cursor)
		return pac_polygon_list.list
	is
		result : pac_polygon_list.list;
		holes : pac_holes.list;
		
		device : type_device_sch renames element (device_cursor);
		packge : pac_package_models.cursor;
		
		rotation : type_rotation_model;
	begin
		if device.appearance = APPEARANCE_PCB then
			packge := get_package_model (device_cursor);
			rotation := device.position.rotation;
			
			holes := get_hole_contours (packge);
		
			if device.flipped = YES then
				mirror_holes (holes);
				rotate_holes (holes, - rotation);
			else
				rotate_holes (holes, + rotation);
			end if;
			
			move_holes (holes, to_distance_relative (device.position.place));
		
			result := to_polygons (holes, fill_tolerance);
		end if;
		return result;
	end get_hole_polygons;
	

	
	function get_holes (
		device_cursor	: in pac_devices_non_electric.cursor)
		return pac_holes.list
	is
		holes : pac_holes.list; -- to be returned
		
		device : type_device_non_electric renames element (device_cursor);
		packge : constant pac_package_models.cursor := get_package_model (device.package_model);

		rotation : type_rotation_model renames device.position.rotation;
	begin
		holes := get_hole_contours (packge);
				
		if device.flipped = YES then
			mirror_holes (holes);
			rotate_holes (holes, - rotation);
		else
			rotate_holes (holes, + rotation);
		end if;
		
		move_holes (holes, to_distance_relative (device.position.place));
		return holes;
	end get_holes;


	
	function get_hole_polygons (
		device_cursor	: in pac_devices_non_electric.cursor)
		return pac_polygon_list.list
	is
		result : pac_polygon_list.list;
		holes : pac_holes.list;
		
		device : type_device_non_electric renames element (device_cursor);
		packge : constant pac_package_models.cursor := get_package_model (device.package_model);
		
		rotation : type_rotation_model renames device.position.rotation;
	begin
		holes := get_hole_contours (packge);
		
		if device.flipped = YES then
			mirror_holes (holes);
			rotate_holes (holes, - rotation);
		else
			rotate_holes (holes, + rotation);
		end if;
		
		move_holes (holes, to_distance_relative (device.position.place));
		
		result := to_polygons (holes, fill_tolerance);
		return result;
	end get_hole_polygons;

	
	
end et_device_query_board;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
