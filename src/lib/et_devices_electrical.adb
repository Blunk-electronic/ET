------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         DEVICES ELECTRICAL                               --
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
-- <http://www.gnu.org/licenses/>.   
------------------------------------------------------------------------------

--   For correct displaying set tab width in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.text_io;				use ada.text_io;
with ada.exceptions;

with et_contour_to_polygon;
with et_string_processing;			use et_string_processing;

with et_symbol_library;
with et_symbol_text;
with et_symbol_name;



package body et_devices_electrical is

	
	function get_unit_names_deployed (
		device : in type_device_sch)
		return pac_unit_names.list
	is 
		result : pac_unit_names.list;

		procedure query_unit (c : in pac_units.cursor) is begin
			result.append (key (c));
		end;

	begin
		device.units.iterate (query_unit'access);
		return result;
	end;

	



	
	function get_unit_count (
		device : in type_device_sch)
		return natural
	is 
		result : natural;
		cursor : pac_devices_lib.cursor;
	begin
		-- Get a cursor to the device in the library.
		-- Map from schematic cursor to library cursor:
		cursor := get_device_model (device);

		-- Get the number of units from the device model:
		result := get_unit_count (cursor);

		return result;
	end;

	

	
	function get_unit_count_deployed (
		device : in type_device_sch)
		return natural
	is begin
		return natural (device.units.length);
	end;


	

	function get_device_model_file (
		device : type_device_sch)
		return pac_device_model_file.bounded_string
	is begin
		return device.model;
	end get_device_model_file;


	

	function get_device_model (
		device : in type_device_sch)
		return pac_devices_lib.cursor
	is
		use et_device_model_names;
		model_file : pac_device_model_file.bounded_string;
	begin
		-- The name of the device model file is THE link
		-- from device in schematic to device in library:
		model_file := get_device_model_file (device);

		-- Locate the device model in the library:
		return get_device_model_cursor (model_file);
	end get_device_model;

	

	

	function get_device_model_file (
		device : pac_devices_sch.cursor)
		return pac_device_model_file.bounded_string
	is begin
		return get_device_model_file (element (device));
	end get_device_model_file;

	


	function get_device_model (
		device : in pac_devices_sch.cursor)
		return pac_devices_lib.cursor
	is
		use et_device_model_names;
		model_file : pac_device_model_file.bounded_string;
	begin
		-- The name of the device model file is THE link
		-- from device in schematic to device in library:
		model_file := get_device_model_file (device);

		-- Locate the device model in the library:
		return get_device_model_cursor (model_file);
	end get_device_model;


	

	function locate_unit (
		device	: in type_device_sch;
		unit	: in pac_unit_name.bounded_string)
		return pac_units.cursor
	is begin
		return find (device.units, unit);
	end;


	


	function to_string (
		device_name		: in type_device_name; -- IC45
		unit_name		: in pac_unit_name.bounded_string; -- C
		query_result	: in type_unit_query)
		return string 
	is 
		use pac_unit_name;
	begin
		if query_result.exists then
			if get_length (unit_name) > 0 then
				return "Location of device " & to_string (device_name)
					& " unit " & to_string (unit_name)
					& " :" & to_string (query_result.position);
			else
				return "Location of device " & to_string (device_name)
					& " :" & to_string (query_result.position);
			end if;
		else
			return "device " & to_string (device_name)
				& " unit " & to_string (unit_name)
				& " does not exist !";
		end if;
	end to_string;





	function get_unit_names_deployed (
		device : in pac_devices_sch.cursor)
		return pac_unit_names.list
	is begin
		return get_unit_names_deployed (element (device));
	end;


	
	

	
	function get_unit_count (
		device : in pac_devices_sch.cursor)
		return type_unit_count
	is 
		result : type_unit_count := 1;
		
		cursor : pac_devices_lib.cursor;
	begin
		-- Get a cursor to the device in the library.
		-- Map from schematic cursor to library cursor:
		cursor := get_device_model (device);

		-- Get the number of units from the device model:
		result := get_unit_count (cursor);
		
		return result;
	end get_unit_count;



	

	function get_unit_count_deployed (
		device : in pac_devices_sch.cursor)
		return natural
	is begin
		return get_unit_count_deployed (element (device));
	end;


	

	function get_device_name (
		device : in pac_devices_sch.cursor)
		return type_device_name
	is begin
		return key (device);
	end get_device_name;


	

	function get_device_name (
		device : in pac_devices_sch.cursor)
		return string
	is begin
		return to_string (key (device));
	end get_device_name;





	function get_full_name (
		device	: in pac_devices_sch.cursor;
		unit	: in pac_units.cursor)
		return string
	is
		d_name : string := get_device_name (device);
		u_name : string := get_unit_name (unit);

		u_count : type_unit_count;
	begin
		u_count := get_unit_count (device);

		if u_count > 1 then
			return d_name & device_unit_separator & u_name; -- IC3,A
		else
			return d_name; -- IC3
		end if;
	end get_full_name;


	


	
	function get_full_name (
		device		: in type_device_name;
		unit		: in pac_unit_name.bounded_string;
		unit_count	: in type_unit_count)
		return string 
	is begin
		if unit_count > 1 then
			return to_string (device) 
				& device_unit_separator 
				& pac_unit_name.to_string (unit);
		else
			return to_string (device);
		end if;
	end get_full_name;



	
	procedure device_name_in_use (
		name : in type_device_name)
	is 
		use et_logging;
	begin
		log (ERROR, "Name " & enclose_in_quotes (to_string (name)) &
			 " already used by another electrical device !",
			 console => true);
		
		raise constraint_error;
	end device_name_in_use;




	procedure log_package_position (
		device_cursor	: in pac_devices_sch.cursor;
		log_threshold	: in type_log_level) 
	is
		use et_pcb_sides;		
		use et_board_geometry;
		use et_board_geometry.pac_geometry_2;
		use et_board_coordinates;
		use pac_devices_sch;
		use et_device_appearance;
	begin
		if element (device_cursor).appearance = APPEARANCE_PCB then
			log (text => "location in board:" & 
				to_string (element (device_cursor).position.place) &
				" face" & 
				to_string (get_face (element (device_cursor).position)),
				level => log_threshold);
		end if;
	end;


	


	function get_position (
		device	: in pac_devices_sch.cursor; -- R2
		unit	: in pac_units.cursor)
		return type_object_position
	is
		unit_position : type_object_position;

		
		procedure query_unit (
			device_name	: in type_device_name;
			device		: in type_device_sch)
		is begin
			-- get the coordinates of the unit
			unit_position := element (unit).position;
		end query_unit;
		
		
	begin
		query_element (
			position	=> device,
			process		=> query_unit'access);

		return unit_position;
	end get_position;





	function get_sheet (
		device	: in pac_devices_sch.cursor; -- R2
		unit	: in pac_units.cursor) -- A, B, C
		return type_sheet
	is 
		position : type_object_position;
	begin
		position := get_position (device, unit);
		return get_sheet (position);
	end get_sheet;

	


	
	function get_unit_position (
		device_cursor	: in pac_devices_sch.cursor;
		unit_name		: in pac_unit_name.bounded_string)
		return type_unit_query
	is 
		exists : boolean := false;

		position : type_object_position;
		
		
		procedure query_device (
			device_name	: in type_device_name;
			device		: in type_device_sch)
		is 
			unit_cursor : pac_units.cursor;
		begin
			-- Locate the given unit in the device.
			-- If the unit does not exist, then
			-- unit_cursor will be pointing to it:
			unit_cursor := device.units.find (unit_name);

			-- Get the coordinates of the unit.
			if has_element (unit_cursor) then
				position := get_position (unit_cursor);
				exists := true;
			end if;
		end query_device;
		

	begin
		query_element (device_cursor, query_device'access);
		
		if exists then
			return (exists => true, position => position);
		else
			return (exists => false);
		end if;
	end get_unit_position;



	


	function get_unit_positions (
		device_cursor : in pac_devices_sch.cursor) 
		return pac_unit_positions.map 
	is
		-- temporarily storage of unit coordinates:
		positions : pac_unit_positions.map;
		
		procedure get_positions (
			device_name : in type_device_name;
			device		: in type_device_sch) 
		is begin
			positions := get_unit_positions (device.units);
		end;

	begin
		pac_devices_sch.query_element (
			position	=> device_cursor,
			process		=> get_positions'access);

		return positions;
	end;

	


	
	procedure log_unit_positions (
		positions 		: in pac_unit_positions.map;
		log_threshold	: in type_log_level) 
	is
		
		procedure write (cursor : in pac_unit_positions.cursor) is 
			use pac_unit_name;
		begin
			log (text => 
				"unit " &
				to_string (pac_unit_positions.key (cursor)) & -- unit name
				to_string (position => pac_unit_positions.element (cursor)), -- sheet x y
				level => log_threshold);
		end;
		
	begin
		log (text => "location(s) in schematic:", level => log_threshold);
		log_indentation_up;
		pac_unit_positions.iterate (positions, write'access);
		log_indentation_down;
	end;





	function get_port_positions (
		device	: in pac_devices_sch.cursor;
		unit	: in pac_units.cursor)
		return pac_points.list
	is
		-- This list of location vectors (x/y-positions) 
		-- will be returned to the caller:
		result : pac_points.list;
		
		-- The given device and unit exist in the schematic.
		-- Both provide information about the name and position
		-- of device and unit in the schematic and the associated
		-- device model. The device model in turn provides information
		-- about the ports and their default positions.
		-- So the device model is the link between schematic unit
		-- and port positions.

		use pac_devices_lib;

		-- This cursor points to the device model in the device library:
		device_cursor_lib : pac_devices_lib.cursor := get_device_model (device);
		-- CS: constraint_error will arise here if no associated device exists.

		-- In order to locate a unit inside the device model,
		-- the actual name of the unit must be known.
		-- So we translate the given unit cursor to a unit name
		-- like A, C, IO_BANK_1:
		unit_name : pac_unit_name.bounded_string := key (unit);


		
		procedure query_internal_units (
			model_name		: in pac_device_model_file.bounded_string;
			device_model	: in type_device_model) 
		is
			unit_cursor : pac_units_internal.cursor;			
		begin
			-- Locate the given unit in the device model
			-- among the internal units. If it has been found,
			-- the the unit_cursor will be set so that it points
			-- to the internal unit:
			locate_internal (device_model, unit_name, unit_cursor);

			-- Get the port positions of the internal unit.
			result := get_port_positions (unit_cursor);
		end query_internal_units;


		
		procedure query_external_units (
			model_name		: in pac_device_model_file.bounded_string;
			device_model	: in type_device_model) 
		is
			unit_cursor : pac_units_external.cursor;
		begin
			-- Locate the given unit in the device model
			-- among the external units. If it has been found,
			-- the the unit_cursor will be set so that it points
			-- to the external unit:
			locate_external (device_model, unit_name, unit_cursor);

			-- Get the port positions of the external unit.
			result := get_port_positions (unit_cursor);
		end query_external_units;


		-- Get the position of the given unit in the schematic:
		unit_position : type_object_position := get_position (unit);
		
	begin
		-- Locate the given unit among the external units 
		-- in the device model. Since it is most likely that
		-- the unit is among the external units, we do this step first:
		query_element (device_cursor_lib, query_external_units'access);

		-- If no positions found, the the given unit either does not
		-- exist or is not external. In that case the unit must
		-- be located among the internal units:
		if get_length (result) = 0 then

			-- Query internal units of device model:
			query_element (device_cursor_lib, query_internal_units'access);
		end if;

		-- Rotate the port positions by the rotation of the unit:
		rotate_points (result, get_rotation (unit_position));

		-- CS mirror ?
		
		-- Move the port positions by the position of the unit:		
		move_points (result, get_place (unit_position));
		
		return result;
	end get_port_positions;



	
	

	function get_ports_of_unit (
		device_cursor	: in pac_devices_sch.cursor;
		unit_name		: in pac_unit_name.bounded_string)
		return pac_ports.map 
	is
		ports : pac_ports.map; -- to be returned
		device_cursor_lib : pac_devices_lib.cursor;
	begin
		device_cursor_lib := get_device_model (device_cursor);
		ports := get_ports_of_unit (device_cursor_lib, unit_name);
		
		return ports;
	end get_ports_of_unit;





	function get_ports_of_unit (
		device_cursor	: in pac_devices_sch.cursor;
		unit_cursor		: in pac_units.cursor)
		return pac_ports.map
	is
		ports : pac_ports.map;

		position : type_object_position;
		rotation : type_rotation_model;
	begin
		-- Get the default positions as described in the 
		-- device model:
		ports := get_ports_of_unit (device_cursor, key (unit_cursor));

		
		-- Rotate and move the ports according to the 
		-- actual rotation and position of the unit in the schematic:
		position := get_position (unit_cursor);
		rotation := get_rotation (position);

		if rotation /= zero_rotation then
			rotate_ports (ports, rotation);
		end if;

		move_ports (ports, position);
		
		return ports;
	end get_ports_of_unit;



	
	
	function get_position (
		device_cursor	: in pac_devices_sch.cursor) -- IC45
		return et_board_coordinates.type_package_position
	is begin
		return element (device_cursor).position;
	end get_position;


	
	function get_position (
		device_cursor	: in pac_devices_sch.cursor) -- IC45
		return et_board_geometry.pac_geometry_2.type_vector_model
	is begin
		return get_position (device_cursor).place;
	end get_position;

	

	
	
	function get_face (
		device_cursor	: in pac_devices_sch.cursor) -- IC45
		return type_face
	is 
		position : et_board_coordinates.type_package_position;
		face : type_face;
	begin
		position := element (device_cursor).position;
		face := et_board_coordinates.get_face (position);
		return face;
	end get_face;





	function get_all_terminals (
		device_cursor	: in pac_devices_sch.cursor) -- IC45
		return pac_terminals.map
	is
		use pac_package_models;
		package_model : constant pac_package_models.cursor := 
			get_package_model (device_cursor);
	begin
		return element (package_model).terminals;
	end get_all_terminals;





	function get_position (
		device		: in pac_devices_sch.cursor; -- R2
		unit		: in pac_units.cursor;
		category	: in type_placeholder_meaning)
		return type_vector_model
	is
		placeholder_position : type_vector_model; -- to be returned

		unit_position : type_object_position;

		
		procedure query_unit (
			device_name	: in type_device_name;
			device		: in type_device_sch)
		is begin
			-- get the coordinates of the unit
			unit_position := element (unit).position;

			-- get the coordinates of the placeholder:
			case category is
				when NAME =>
					placeholder_position := element (unit).placeholders.name.position;

				when PURPOSE =>
					placeholder_position := element (unit).placeholders.purpose.position;

				when VALUE =>
					placeholder_position := element (unit).placeholders.value.position;
			end case;

			move_by (placeholder_position, unit_position.place);
			
		end query_unit;

		
	begin
		query_element (
			position	=> device,
			process		=> query_unit'access);

		return placeholder_position;
	end get_position;



	
	

-- CONDUCTOR OBJECTS:
	

	function get_conductor_objects (
		device_cursor	: in pac_devices_sch.cursor;
		layer_category	: in type_signal_layer_category)
		return type_conductor_objects
	is
		conductors : type_conductor_objects; -- to be returned
		device : type_device_sch renames element (device_cursor);
		packge : pac_package_models.cursor;

		use et_board_geometry;
		use et_board_geometry.pac_geometry_2;
	begin
		if device.appearance = APPEARANCE_PCB then
			packge := get_package_model (device_cursor);

			if layer_category /= INNER then -- non-electric conductor objects exist in outer layers only
				case get_face (device_cursor) is
					when TOP =>
						conductors := get_conductor_objects (packge, layer_category);
						rotate_conductor_objects (conductors, + device.position.rotation);

					when BOTTOM =>
						conductors := get_conductor_objects (packge, invert_category (layer_category));
						mirror_conductor_objects (conductors);
						rotate_conductor_objects (conductors, - device.position.rotation);
				end case;

				move_conductor_objects (conductors, device.position.place);
			end if;
		end if;
		
		return conductors;
	end get_conductor_objects;
	

	




	function get_conductor_polygons (
		device_cursor	: in pac_devices_sch.cursor;
		layer_category	: in type_signal_layer_category)
		return et_board_geometry.pac_polygons.pac_polygon_list.list
	is
		use et_board_geometry;
		use et_board_geometry.pac_polygons;
		result : pac_polygon_list.list;
		use et_contour_to_polygon;
		
		device : type_device_sch renames element (device_cursor);
		packge : pac_package_models.cursor;
		conductors : type_conductor_objects; -- non-electrical

		-- use et_board_coordinates;
		-- use et_board_coordinates.pac_geometry_2;

	begin
		if device.appearance = APPEARANCE_PCB then
			packge := get_package_model (device_cursor);

			if layer_category /= INNER then -- non-electric conductor objects exist in outer layers only
				case get_face (device_cursor) is
					when TOP =>
						conductors := get_conductor_objects (packge, layer_category);
						rotate_conductor_objects (conductors, + device.position.rotation);

					when BOTTOM =>
						conductors := get_conductor_objects (packge, invert_category (layer_category));
						mirror_conductor_objects (conductors);
						rotate_conductor_objects (conductors, - device.position.rotation);
				end case;

				move_conductor_objects (conductors, device.position.place);

				-- convert conductor objects to polygons:
				result := to_polygons (conductors, fill_tolerance);
			end if;
		end if;
		
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

		use et_board_geometry;
		use et_board_geometry.pac_geometry_2;
	begin
		if device.appearance = APPEARANCE_PCB then
			packge := get_package_model (device_cursor);
				
			if layer_category /= INNER then -- route restrict objects exist in outer layers only
				case get_face (device_cursor) is
					when TOP =>
						restrict := get_route_restrict_objects (packge, layer_category);
						rotate_route_restrict_objects (restrict, + device.position.rotation);
					when BOTTOM =>
						restrict := get_route_restrict_objects (packge, invert_category (layer_category));
						mirror_route_restrict_objects (restrict);
						rotate_route_restrict_objects (restrict, - device.position.rotation);
				end case;

				move_route_restrict_objects (restrict, device.position.place);
			end if;
		end if;

		return restrict;
	end get_route_restrict_objects;


	
	

	function get_route_restrict_polygons (
		device_cursor	: in pac_devices_sch.cursor;
		layer_category	: in type_signal_layer_category)
		return et_board_geometry.pac_polygons.pac_polygon_list.list
	is
		
		device : type_device_sch renames element (device_cursor);
		packge : pac_package_models.cursor;

		use et_route_restrict.packages;
		restrict : type_one_side;

		use et_board_geometry;
		use et_board_geometry.pac_geometry_2;
		use et_board_geometry.pac_polygons;

		result : pac_polygon_list.list;
	begin
		if device.appearance = APPEARANCE_PCB then
			packge := get_package_model (device_cursor);
				
			if layer_category /= INNER then -- route restrict objects exist in outer layers only
				case get_face (device_cursor) is
					when TOP =>
						restrict := get_route_restrict_objects (packge, layer_category);
						rotate_route_restrict_objects (restrict, + device.position.rotation);
					when BOTTOM =>
						restrict := get_route_restrict_objects (packge, invert_category (layer_category));
						mirror_route_restrict_objects (restrict);
						rotate_route_restrict_objects (restrict, - device.position.rotation);
				end case;

				move_route_restrict_objects (restrict, device.position.place);

				-- convert restrict objects to polygons:
				result := to_polygons (restrict, fill_tolerance);
			end if;
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

		use et_board_geometry;
		use et_board_geometry.pac_geometry_2;

	begin
		if device.appearance = APPEARANCE_PCB then
			packge := get_package_model (device_cursor);
				
			if layer_category /= INNER then -- via restrict objects exist in outer layers only
				case get_face (device_cursor) is
					when TOP =>
						restrict := get_via_restrict_objects (packge, layer_category);
						rotate_via_restrict_objects (restrict, + device.position.rotation);

					when BOTTOM =>
						restrict := get_via_restrict_objects (packge, invert_category (layer_category));
						mirror_via_restrict_objects (restrict);
						rotate_via_restrict_objects (restrict, - device.position.rotation);
				end case;

				move_via_restrict_objects (restrict, device.position.place);
			end if;
		end if;

		return restrict;
	end get_via_restrict_objects;


	



	
-- KEEPOUT
	
	function get_keepout_objects (
		device_cursor	: in pac_devices_sch.cursor;
		face			: in type_face)
		return type_keepout
	is
		use et_board_geometry;
		use et_board_geometry.pac_geometry_2;

		result : type_keepout;
		device : type_device_sch renames element (device_cursor);
		packge : pac_package_models.cursor;
		rotation : et_board_geometry.type_rotation_model;

	begin
		if device.appearance = APPEARANCE_PCB then
			packge := get_package_model (device_cursor);
			rotation := device.position.rotation;
			
			case face is
				when TOP =>
					case get_face (device_cursor) is
						when TOP =>
							result := get_keepout_objects (packge, TOP);
							rotate_keepout_objects (result, + rotation);

						when BOTTOM =>
							result := get_keepout_objects (packge, BOTTOM);
							mirror_keepout_objects (result);
							rotate_keepout_objects (result, - rotation);
					end case;

				when BOTTOM =>
					case get_face (device_cursor) is
						when TOP =>
							result := get_keepout_objects (packge, BOTTOM);
							rotate_keepout_objects (result, + rotation);

						when BOTTOM =>
							result := get_keepout_objects (packge, TOP);
							mirror_keepout_objects (result);
							rotate_keepout_objects (result, - rotation);
					end case;
			end case;
		end if;
		
		move_keepout_objects (result, device.position.place);
		return result;
	end get_keepout_objects;




	


	
	
-- STENCIL
	
	function get_stencil_objects (
		device_cursor	: in pac_devices_sch.cursor;
		face			: in type_face)
		return type_stencil
	is
		use et_board_geometry;
		use et_board_geometry.pac_geometry_2;
		
		result : type_stencil;
		device : type_device_sch renames element (device_cursor);
		packge : pac_package_models.cursor;
		rotation : et_board_geometry.type_rotation_model;
	begin
		if device.appearance = APPEARANCE_PCB then
			packge := get_package_model (device_cursor);
			rotation := device.position.rotation;
			
			case face is
				when TOP =>
					case get_face (device_cursor) is
						when TOP =>
							result := get_stencil_objects (packge, TOP);
							rotate_stencil_objects (result, + rotation);
						when BOTTOM =>
							result := get_stencil_objects (packge, BOTTOM);
							mirror_stencil_objects (result);
							rotate_stencil_objects (result, - rotation);
					end case;

				when BOTTOM =>
					case get_face (device_cursor) is
						when TOP =>
							result := get_stencil_objects (packge, BOTTOM);
							rotate_stencil_objects (result, + rotation);
						when BOTTOM =>
							result := get_stencil_objects (packge, TOP);
							mirror_stencil_objects (result);
							rotate_stencil_objects (result, - rotation);
					end case;
			end case;
		end if;
		
		move_stencil_objects (result, device.position.place);
		return result;
	end get_stencil_objects;

	

	



	

-- STOPMASK:

	function get_stopmask_objects (
		device_cursor	: in pac_devices_sch.cursor;
		face			: in type_face)
		return type_stopmask
	is
		use et_board_geometry;
		use et_board_geometry.pac_geometry_2;
		
		result : type_stopmask;
		device : type_device_sch renames element (device_cursor);
		packge : pac_package_models.cursor;
		rotation : et_board_geometry.type_rotation_model;

		use et_stopmask.packages;
	begin
		if device.appearance = APPEARANCE_PCB then
			packge := get_package_model (device_cursor);
			rotation := device.position.rotation;

			case face is
				when TOP =>
					case get_face (device_cursor) is
						when TOP =>
							result := get_stopmask_objects (packge, TOP);
							rotate_stopmask_objects (result, + rotation);
							
						when BOTTOM =>
							result := get_stopmask_objects (packge, BOTTOM);
							mirror_stopmask_objects (result);
							rotate_stopmask_objects (result, - rotation);
					end case;

				when BOTTOM =>
					case get_face (device_cursor) is
						when TOP =>
							result := get_stopmask_objects (packge, BOTTOM);
							rotate_stopmask_objects (result, + rotation);

						when BOTTOM =>
							result := get_stopmask_objects (packge, TOP);
							mirror_stopmask_objects (result);
							rotate_stopmask_objects (result, - rotation);
					end case;
			end case;

			move_stopmask_objects (result, device.position.place);			
		end if;

		return result;
	end get_stopmask_objects;






-- PLACEHOLDERS
	

	function to_placeholder_content (
		device_cursor	: in pac_devices_sch.cursor;
		placeholder		: in et_device_placeholders.packages.type_placeholder)
		return et_text.pac_text_content.bounded_string 
	is
		device : type_device_sch renames element (device_cursor);

		use et_text;
		result : pac_text_content.bounded_string;

		use et_device_placeholders;
	begin
		case placeholder.meaning is
			when NAME 		=> result := to_content (to_string (key (device_cursor)));
			when VALUE		=> result := to_content (to_string (device.value));
			when PURPOSE	=> result := to_content (to_string (device.purpose));
		end case;
		
		return result;
	end to_placeholder_content;





	function get_default_text_positions (
		device_cursor	: in pac_devices_sch.cursor;
		unit_name		: in pac_unit_name.bounded_string)
		return type_default_text_positions 
	is		
		use et_device_appearance;
		use pac_devices_lib;

		-- The positions to be returned depend on the appearance of the requested device:
		result : type_default_text_positions (element (device_cursor).appearance); -- to be returned
		
		model : pac_device_model_file.bounded_string; -- ../libraries/devices/transistor/pnp.dev
		device_cursor_lib : pac_devices_lib.cursor;

		use et_symbol_text;
		use pac_symbol_texts;

		
		procedure query_text (c : in pac_symbol_texts.cursor) is 
		-- Appends a text position (x/y) the the result.
			use pac_text_positions;
		begin
			append (result.texts, element (c).position);
		end;

		
		-- Indicates whether the unit is internal or external:
		unit_status : type_unit_ext_int := EXT;

		
		procedure query_internal_units (
			model	: in pac_device_model_file.bounded_string;
			device	: in type_device_model) 
		is
			use pac_units_internal;			
			unit_cursor : pac_units_internal.cursor;
		begin
			-- locate the given unit among the internal units
			unit_cursor := find (device.units_internal, unit_name);

			-- if the unit exists among the internal units:
			if unit_cursor /= pac_units_internal.no_element then
				unit_status := INT;
				
				-- Collect the positions of texts and store them in result.text
				-- in the same order as they are listed in symbol.texts:
				iterate (element (unit_cursor).symbol.texts, query_text'access);
				-- CS: constraint_error arises here if unit can not be located.

				-- If it is about a real device, take a copy of the default 
				-- placeholders as they are specified in the symbol model:
				case result.appearance is
					when APPEARANCE_PCB =>
						result.placeholders.name	:= element (unit_cursor).symbol.placeholders.name;
						result.placeholders.value	:= element (unit_cursor).symbol.placeholders.value;
						result.placeholders.purpose	:= element (unit_cursor).symbol.placeholders.purpose;
					when others => null;
				end case;

			else
				unit_status := EXT;
			end if;
		end query_internal_units;

		
		
		procedure query_external_units (
			model	: in pac_device_model_file.bounded_string;
			device	: in type_device_model) 
		is
			use et_symbol_library;
			use et_symbol_name;
			use pac_units_external;
			unit_cursor : pac_units_external.cursor;
			sym_model : pac_symbol_model_file.bounded_string; -- like /libraries/symbols/NAND.sym

			
			procedure query_symbol (
				symbol_name	: in pac_symbol_model_file.bounded_string;
				symbol		: in type_symbol ) 
			is begin
				-- Collect the positions of texts and store them in result.text
				-- in the same order as they are listed in symbol.texts:
				iterate (symbol.texts, query_text'access);

				-- If it is about a real device, take a copy of the default 
				-- placeholders as they are specified in the symbol model:
				case result.appearance is
					when APPEARANCE_PCB =>
						result.placeholders.name 	:= symbol.placeholders.name;
						result.placeholders.value	:= symbol.placeholders.value;
						result.placeholders.purpose	:= symbol.placeholders.purpose;
					when others => null;
				end case;
			end query_symbol;

			
		begin -- query_external_units
			-- locate the given unit among the external units
			unit_cursor := find (device.units_external, unit_name);

			-- Fetch the symbol model file of the external unit.
			-- If unit could not be located then it must be internal.
			if unit_cursor /= pac_units_external.no_element then
				unit_status := EXT;
				
				sym_model := element (unit_cursor).model;

				-- Fetch the ports of the external unit.
				-- CS: constraint_error arises here if symbol model could not be located.
				pac_symbols.query_element (
					position	=> pac_symbols.find (symbol_library, sym_model),
					process		=> query_symbol'access);
			else
				unit_status := INT;
			end if;
			
		end query_external_units;

		
	begin

		-- Fetch the model name of the given device. 
		model := pac_devices_sch.element (device_cursor).model;

		-- Get cursor to device in device library (the model name is the key into the device library).
		-- CS: constraint_error will arise here if no associated device exists.
		device_cursor_lib := find (device_library, model);

		-- Query external units of device (in library). It is most likely that
		-- the unit is among the external units:
		query_element (
			position	=> device_cursor_lib,
			process		=> query_external_units'access);

		-- If unit could not be found among external units then look up the internal units:
		if unit_status = INT then

			-- Query internal units of device (in library):
			query_element (
				position	=> device_cursor_lib,
				process		=> query_internal_units'access);
		end if;
		
		-- CS raise error if unit could not be located at all.
			
		return result;

		exception
			when event: others =>
				log_indentation_reset;
				log (text => ada.exceptions.exception_information (event), console => true);
				raise;
		
	end get_default_text_positions;




	

	
-- SILKSCREEN

	function get_silkscreen_objects (
		device_cursor	: in pac_devices_sch.cursor;
		face			: in type_face)
		return type_silkscreen
	is
		use et_board_geometry;
		use et_board_geometry.pac_geometry_2;
		use et_board_text;
		
		result : type_silkscreen;
		device : type_device_sch renames element (device_cursor);
		packge : pac_package_models.cursor;
		rotation : et_board_geometry.type_rotation_model;

		use et_silkscreen.packages;
		silkscreen : et_silkscreen.packages.type_silkscreen_package;

		
		-- Converts the placeholders to a list of regular texts
		-- and appends them to the silkscreen.texts:
		procedure convert_placeholders_to_texts is
			use et_device_placeholders.packages;
			use pac_placeholders;

			procedure query_placeholder (c : in pac_placeholders.cursor) is
				ph : type_placeholder renames element (c);
				use pac_text_board;
				text : type_silk_text := (type_text_fab (ph) with others => <>);
				use et_text;
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
					case get_face (device_cursor) is
						when TOP =>
							silkscreen := get_silkscreen_objects (packge, TOP);

							-- overwrite the default placeholders: -- CS see spec of this function
							silkscreen.placeholders := device.text_placeholders.silkscreen.top;
							convert_placeholders_to_texts;
							rotate_silkscreen_objects (silkscreen, + rotation);
							
						when BOTTOM =>
							silkscreen := get_silkscreen_objects (packge, BOTTOM);
							
							-- overwrite the default placeholders: -- CS see spec of this function
							silkscreen.placeholders := device.text_placeholders.silkscreen.bottom;
							convert_placeholders_to_texts;
							mirror_silkscreen_objects (silkscreen);
							rotate_silkscreen_objects (silkscreen, - rotation);
					end case;

					
				when BOTTOM =>
					case get_face (device_cursor) is
						when TOP =>
						   silkscreen := get_silkscreen_objects (packge, BOTTOM);
						
							-- overwrite the default placeholders: -- CS see spec of this function
							silkscreen.placeholders := device.text_placeholders.silkscreen.bottom;
							convert_placeholders_to_texts;
							rotate_silkscreen_objects (silkscreen, + rotation);
							
						when BOTTOM =>
							silkscreen := get_silkscreen_objects (packge, TOP);
							
							-- overwrite the default placeholders: -- CS see spec of this function
							silkscreen.placeholders := device.text_placeholders.silkscreen.top;
							convert_placeholders_to_texts;
							mirror_silkscreen_objects (silkscreen);
							rotate_silkscreen_objects (silkscreen, - rotation);
					end case;
			end case;

			move_silkscreen_objects (silkscreen, device.position.place);
		end if;

		result := type_silkscreen (silkscreen);		
		return result;
	end get_silkscreen_objects;
	


	
	



	

	
-- ASSEMBLY DOCUMENTATION:
	
	function get_assy_doc_objects (
		device_cursor	: in pac_devices_sch.cursor;
		face			: in type_face)
		return type_assy_doc
	is
		use et_board_geometry;
		use et_board_geometry.pac_geometry_2;
		use et_board_text;

		result : type_assy_doc;
		device : type_device_sch renames element (device_cursor);
		packge : pac_package_models.cursor;
		rotation : et_board_geometry.type_rotation_model;

		use et_assy_doc.packages;
		assy_doc : et_assy_doc.packages.type_assy_doc_package;


		-- Converts the placeholders to a list of regular texts
		-- and appends them to the assy_doc.texts:
		procedure convert_placeholders_to_texts is
			use et_device_placeholders.packages;
			use pac_placeholders;

			procedure query_placeholder (c : in pac_placeholders.cursor) is
				ph : type_placeholder renames element (c);
				use pac_text_board;
				text : type_doc_text := (type_text_fab (ph) with others => <>);
				use et_text;
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
					case get_face (device_cursor) is
						when TOP =>
							assy_doc := get_assy_doc_objects (packge, TOP);
							
							-- overwrite the default placeholders: -- CS see spec of this function
							assy_doc.placeholders := device.text_placeholders.assy_doc.top;
							convert_placeholders_to_texts;
							rotate_assy_doc_objects (assy_doc, + rotation);

						when BOTTOM =>
							assy_doc := get_assy_doc_objects (packge, BOTTOM);
							
							-- overwrite the default placeholders: -- CS see spec of this function
							assy_doc.placeholders := device.text_placeholders.assy_doc.bottom;
							convert_placeholders_to_texts;
							mirror_assy_doc_objects (assy_doc);
							rotate_assy_doc_objects (assy_doc, - rotation);
					end case;

				when BOTTOM =>
					case get_face (device_cursor) is
						when TOP =>
							assy_doc := get_assy_doc_objects (packge, BOTTOM);

							-- overwrite the default placeholders: -- CS see spec of this function
							assy_doc.placeholders := device.text_placeholders.assy_doc.bottom;
							convert_placeholders_to_texts;
							rotate_assy_doc_objects (assy_doc, + rotation);

						when BOTTOM =>
							assy_doc := get_assy_doc_objects (packge, TOP);

							-- overwrite the default placeholders: -- CS see spec of this function
							assy_doc.placeholders := device.text_placeholders.assy_doc.top;
							convert_placeholders_to_texts;
							mirror_assy_doc_objects (assy_doc);
							rotate_assy_doc_objects (assy_doc, - rotation);
					end case;
			end case;

			move_assy_doc_objects (assy_doc, device.position.place);
		end if;

		result := type_assy_doc (assy_doc);
		return result;
	end get_assy_doc_objects;
	




-- HOLES
	
	function get_holes (
		device_cursor	: in pac_devices_sch.cursor)
		return pac_holes.list
	is
		use et_board_geometry;
		use et_board_geometry.pac_geometry_2;
		
		holes : pac_holes.list; -- to be returned
		
		device : type_device_sch renames element (device_cursor);
		packge : pac_package_models.cursor;

		rotation : et_board_geometry.type_rotation_model;
	begin
		if device.appearance = APPEARANCE_PCB then
			packge := get_package_model (device_cursor);
			rotation := device.position.rotation;
			
			holes := get_hole_contours (packge);
					
			case get_face (device_cursor) is
				when TOP =>
					mirror_holes (holes);
					rotate_holes (holes, - rotation);
					
				when BOTTOM =>
					rotate_holes (holes, + rotation);
			end case;
		
			move_holes (holes, device.position.place);
		end if;
		
		return holes;		
	end get_holes;


	
	
	function get_hole_polygons (
		device_cursor	: in pac_devices_sch.cursor)
		return et_board_geometry.pac_polygons.pac_polygon_list.list
	is
		holes : pac_holes.list;
		
		device : type_device_sch renames element (device_cursor);
		packge : pac_package_models.cursor;
		
		use et_board_geometry;
		use et_board_geometry.pac_geometry_2;
		use et_board_geometry.pac_polygons;

		rotation : et_board_geometry.type_rotation_model;
		
		result : pac_polygon_list.list;
	begin
		if device.appearance = APPEARANCE_PCB then
			packge := get_package_model (device_cursor);
			rotation := device.position.rotation;
			
			holes := get_hole_contours (packge);
		
			case get_face (device_cursor) is
				when TOP =>
					mirror_holes (holes);
					rotate_holes (holes, - rotation);

				when BOTTOM =>
					rotate_holes (holes, + rotation);
			end case;
			
			move_holes (holes, device.position.place);
		
			result := to_polygons (holes, fill_tolerance);
		end if;
		return result;
	end get_hole_polygons;
	

	

	
	
	procedure set_selected (
		device : in out type_device_sch)
	is begin
		if device.appearance = APPEARANCE_PCB then
			set_selected (device.status);
		end if;
	end;

	
	procedure clear_selected (
		device : in out type_device_sch)
	is begin
		if device.appearance = APPEARANCE_PCB then
			clear_selected (device.status);
		end if;
	end;


	function is_selected (
		device : in type_device_sch)
		return boolean
	is begin
		if device.appearance = APPEARANCE_PCB then
			if is_selected (device.status) then
				return true;
			else
				return false;
			end if;
		else
			return false;
		end if;
	end;	
	

	
	procedure set_proposed (
		device : in out type_device_sch)
	is begin
		if device.appearance = APPEARANCE_PCB then
			set_proposed (device.status);
		end if;
	end;
	

	procedure clear_proposed (
		device : in out type_device_sch)
	is begin
		if device.appearance = APPEARANCE_PCB then
			clear_proposed (device.status);
		end if;
	end;

	
	
	function is_proposed (
		device : in type_device_sch)
		return boolean
	is begin
		if device.appearance = APPEARANCE_PCB then
			if is_proposed (device.status) then
				return true;
			else
				return false;
			end if;
		else
			return false;
		end if;
	end;



	procedure set_moving (
		device : in out type_device_sch)
	is begin
		if device.appearance = APPEARANCE_PCB then
			set_moving (device.status);
		end if;
	end;
	

	procedure clear_moving (
		device : in out type_device_sch)
	is begin
		if device.appearance = APPEARANCE_PCB then
			clear_moving (device.status);
		end if;
	end;

	
	
	function is_moving (
		device : in type_device_sch)
		return boolean
	is begin
		if device.appearance = APPEARANCE_PCB then
			if is_moving (device.status) then
				return true;
			else
				return false;
			end if;
		else
			return false;
		end if;
	end;





	

	procedure modify_status (
		device		: in out type_device_sch;
		operation	: in type_status_operation)
	is begin
		modify_status (device.status, operation);
	end modify_status;

	

	procedure reset_status (
		device : in out type_device_sch)
	is begin
		reset_status (device.status);
	end;

	


	function is_real (
		device : in pac_devices_sch.cursor)
		return boolean 
	is 
		use pac_devices_sch;
	begin
		case element (device).appearance is
			when APPEARANCE_PCB		=> return true;
			when APPEARANCE_VIRTUAL	=> return false;
		end case;
	end is_real;
	


	function is_proposed (
		device : in pac_devices_sch.cursor)
		return boolean
	is begin
		if is_proposed (element (device)) then
			return true;
		else
			return false;
		end if;
	end is_proposed;
	

	

	function is_selected (
		device : in pac_devices_sch.cursor)
		return boolean
	is begin
		if is_selected (element (device)) then
			return true;
		else
			return false;
		end if;
	end is_selected;




	function is_moving (
		device : in pac_devices_sch.cursor)
		return boolean
	is begin
		if is_moving (element (device)) then
			return true;
		else
			return false;
		end if;
	end is_moving;

	


	procedure iterate (
		devices	: in pac_devices_sch.map;
		process	: not null access procedure (position : in pac_devices_sch.cursor);
		proceed	: not null access boolean)
	is 
		use pac_devices_sch;
		c : pac_devices_sch.cursor := devices.first;
	begin
		while c /= pac_devices_sch.no_element and proceed.all = TRUE loop
			process (c);
			next (c);
		end loop;
	end iterate;
	

	





	

	function get_package_model (
		device : in pac_devices_sch.cursor)
		return pac_package_model_file_name.bounded_string -- libraries/packages/smd/SOT23.pac
	is
		use et_device_model_names;
		device_model		: pac_device_model_file.bounded_string;
		device_cursor_lib	: pac_devices_lib.cursor;
		device_variant		: pac_package_variant_name.bounded_string; -- N, D
	begin
		-- CS: The device is located twice here. Consumes too much time.
		-- The issue may dissolve once devices are stored in a hashed map:
		
		-- load package variant of given device
		device_variant := pac_devices_sch.element (device).variant;
		
		-- load the name of the generic device model
		device_model := pac_devices_sch.element (device).model;
		
		-- locate the generic device model in the device library
		device_cursor_lib := get_device_model_cursor (device_model);
		
		return get_package_model (device_cursor_lib, device_variant);
	end get_package_model;



	
	
	function get_package_model (
		device : in pac_devices_sch.cursor)
		return pac_package_models.cursor
	is
		package_model : constant pac_package_model_file_name.bounded_string :=
			get_package_model (device);  -- libraries/packages/smd/SOT23.pac
	begin
		return get_package_model (package_model);
	end get_package_model;




	function has_real_package (
		device : in pac_devices_sch.cursor) 
		return boolean 
	is
		package_name : pac_package_model_file_name.bounded_string; -- libraries/packages/smd/SOT23.pac
	begin
		-- get the package name of the given device:
		package_name := get_package_model (device);

		-- ask for the package status (real or virtual) and return the result right away:
		return is_real (package_name);
	end has_real_package;

	

	function get_value (
		device : in pac_devices_sch.cursor)
		return pac_device_value.bounded_string 
	is begin
		return pac_devices_sch.element (device).value;
	end get_value;

	

	function get_purpose (
		device : in pac_devices_sch.cursor)
		return pac_device_purpose.bounded_string
	is begin
		return pac_devices_sch.element (device).purpose;
	end get_purpose;

	
	
	function get_partcode (
		device : in pac_devices_sch.cursor)
		return pac_device_partcode.bounded_string
	is begin
		return pac_devices_sch.element (device).partcode;
	end get_partcode;


	
	function get_package_variant (
		device : in pac_devices_sch.cursor)
		return pac_package_variant_name.bounded_string
	is begin
		return pac_devices_sch.element (device).variant;
	end get_package_variant;



	function get_port (
		device		: in pac_devices_sch.cursor;
		terminal	: in et_terminals.pac_terminal_name.bounded_string)
		return type_get_port_result
	is		
		-- CS: 
		-- simplify header as in function get_terminal
		-- use function et_devices.get_unit_and_port
		
		result : type_get_port_result;

		-- Get the cursor to the full device model in the library:
		device_model : constant pac_devices_lib.cursor := 
			get_device_model_cursor (pac_devices_sch.element (device).model);

		-- This is the package variant used by the given device:
		variant_sch : constant pac_package_variant_name.bounded_string :=
			pac_devices_sch.element (device).variant; -- N, D


		use et_device_model;
		use et_device_model_names;

		
		procedure query_model (
			model	: in pac_device_model_file.bounded_string;
			device	: in type_device_model)
		is
			use pac_package_variants;

			-- Locate the package variant of the given device
			-- in the device model:
			variant_lib : constant pac_package_variants.cursor := 
				find (device.variants, variant_sch);

			
			procedure query_terminal_port_map (
				name	: in pac_package_variant_name.bounded_string;
				variant	: in type_package_variant)
			is
				use pac_terminal_port_map;

				-- Locate the terminal in the terminal-port-map
				-- of the device model:
				t : constant pac_terminal_port_map.cursor :=
					find (variant.terminal_port_map, terminal);
			begin
				-- Get the port and unit name (which is what we want):
				if t /= pac_terminal_port_map.no_element then
					result := (
						linked	=> TRUE, 
						unit	=> element (t).unit, 
						port	=> element (t).name);
				else
					-- If the terminal can not be found in the map then
					-- it is not linked to any port.
					result := (linked => FALSE);
				end if;
			end query_terminal_port_map;

			
		begin
			query_element (variant_lib, query_terminal_port_map'access);

			-- CS result := get_unit_and_port (variant_lib, terminal);
		end query_model;

		
	begin
		pac_devices_lib.query_element (device_model, query_model'access);
			
		return result;
	end get_port;

	
	

	function get_terminal (
		device	: in pac_devices_sch.cursor;
		unit	: in pac_unit_name.bounded_string;
		port	: in pac_port_name.bounded_string)
		return et_terminals.pac_terminals.cursor
	is
		use et_terminals;
		use pac_terminals;

		-- Get the cursor to the full device model in the library:
		use pac_devices_lib;
		device_model_lib : constant pac_devices_lib.cursor := get_device_model (device);

		-- This is the name of the package variant used by the given device:
		variant_sch : constant pac_package_variant_name.bounded_string :=
			get_package_variant (device); -- N, D

		-- Get full information about the package variant:
		use pac_package_variants;
		variant_lib : constant pac_package_variants.cursor := 
			get_package_variant (device_model_lib, variant_sch);

		terminal_name : constant pac_terminal_name.bounded_string := 
			get_terminal (variant_lib, unit, port);

		use et_packages;
		use pac_package_models;
		package_cursor : pac_package_models.cursor;

	begin
		-- Get a cursor to the package model:
		package_cursor := get_package_model (element (variant_lib).package_model);
		
		-- Get the cursor to the actual terminal:
		return get_terminal (package_cursor, terminal_name);
	end get_terminal;


	
	--procedure iterate (
		--devices	: in pac_devices_non_electric.map;
		--process	: not null access procedure (position : in pac_devices_non_electric.cursor);
		--proceed	: not null access boolean)
	--is
		--use pac_devices_non_electric;
		--c : pac_devices_non_electric.cursor := devices.first;
	--begin
		--while c /= no_element and proceed.all = TRUE loop
			--process (c);
			--next (c);
		--end loop;
	--end iterate;
	


	
end et_devices_electrical;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
