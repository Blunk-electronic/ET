------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              SCHEMATIC                                   --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2024                                                -- 
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
with ada.strings.maps;			use ada.strings.maps;

with ada.characters.handling;	use ada.characters.handling;
with ada.exceptions;

with et_string_processing;		use et_string_processing;
with et_logging;				use et_logging;

with et_device_model;


package body et_schematic is
	


	procedure set_selected (
		device		: in out type_device_sch)
	is begin
		set_selected (device.status);
	end;

	
	procedure clear_selected (
		device		: in out type_device_sch)
	is begin
		clear_selected (device.status);
	end;


	function is_selected (
		device		: in type_device_sch)
		return boolean
	is begin
		if is_selected (device.status) then
			return true;
		else
			return false;
		end if;
	end;	
	

	
	procedure set_proposed (
		device		: in out type_device_sch)
	is begin
		set_proposed (device.status);
	end;
	

	procedure clear_proposed (
		device		: in out type_device_sch)
	is begin
		clear_proposed (device.status);
	end;

	
	
	function is_proposed (
		device		: in type_device_sch)
		return boolean
	is begin
		if is_proposed (device.status) then
			return true;
		else
			return false;
		end if;
	end;

	

	procedure modify_status (
		device		: in out type_device_sch;
		operation	: in type_status_operation)
	is begin
		case operation.action is
			when SET =>
				null;

			when CLEAR =>
				null;
		end case;
	end modify_status;

	
	

	
	function get_first_strand_on_sheet (
		sheet		: in type_sheet;
		net_cursor	: in pac_nets.cursor)
		return pac_strands.cursor
	is
		use pac_nets;
		strand_cursor : pac_strands.cursor; -- to be returned

		strand_position : et_coordinates_2.type_position := greatest_position;
		
		procedure query_strands (
			net_name	: in pac_net_name.bounded_string;
			net			: in type_net)
		is
			use pac_strands;

			c : pac_strands.cursor := net.strands.first;
		begin			
			while c /= pac_strands.no_element loop

				-- Probe strands on the given sheet only:
				if get_sheet (element (c).position) = sheet then

					if element (c).position < strand_position then
						strand_position := element (c).position;
						strand_cursor := c;
					end if;

				end if;
				
				next (c); -- advance to next strand
			end loop;
		end query_strands;

	begin
		query_element (
			position	=> net_cursor,
			process		=> query_strands'access);

		return strand_cursor;
	end get_first_strand_on_sheet;



	
	function get_first_strand (
		net_cursor	: in pac_nets.cursor)
		return pac_strands.cursor
	is
		use pac_nets;
		strand_cursor : pac_strands.cursor; -- to be returned

		strand_position : et_coordinates_2.type_position := greatest_position;
		
		procedure query_strands (
			net_name	: in pac_net_name.bounded_string;
			net			: in type_net)
		is
			use pac_strands;

			procedure query_strand (c : in pac_strands.cursor) is begin
				if element (c).position < strand_position then
					strand_position := element (c).position;
					strand_cursor := c;
				end if;
			end query_strand;
			
		begin			
			iterate (net.strands, query_strand'access);
		end query_strands;
			
	begin -- get_first_strand
		query_element (
			position	=> net_cursor,
			process		=> query_strands'access);
	
		return strand_cursor;
	end get_first_strand;



	
	function to_label_rotation (direction : in type_stub_direction) 
		return type_rotation_model is
	begin
		case direction is
			when RIGHT	=> return zero_rotation;
			when LEFT	=> return 180.0;
			when UP		=> return 90.0;
			when DOWN	=> return -90.0;
		end case;
	end to_label_rotation;


	
	
	function stub_direction (
		segment	: in pac_net_segments.cursor;
		point	: in type_vector_model)
		return type_stub 
	is
		use pac_net_segments;

		is_stub : boolean := true;
		direction : type_stub_direction;
		orientation : constant type_net_segment_orientation := segment_orientation (segment);
	begin
		case orientation is
			when HORIZONTAL =>
				if get_x (point) >= get_x (element (segment).start_point) and
					get_x (point) >= get_x (element (segment).end_point) then
					direction := RIGHT;
				end if;

				if get_x (point) <= get_x (element (segment).start_point) and
					get_x (point) <= get_x (element (segment).end_point) then
					direction := LEFT;
				end if;
				
			when VERTICAL =>
				if get_y (point) >= get_y (element (segment).start_point) and
					get_y (point) >= get_y (element (segment).end_point) then
					direction := UP;
				end if;

				if get_y (point) <= get_y (element (segment).start_point) and
					get_y (point) <= get_y (element (segment).end_point) then
					direction := DOWN;
				end if;
				
			when SLOPING =>
				is_stub := false;
		end case;

		if is_stub then
			return (is_stub => TRUE, direction => direction);
		else
			return (is_stub => FALSE);
		end if;

	end stub_direction;


	
	
	function get_ports (
		net		: in pac_nets.cursor;
		variant	: in pac_assembly_variants.cursor := pac_assembly_variants.no_element)
		return type_ports 
	is
		result : type_ports; -- to be returned

		use pac_nets;
		use pac_strands;
		use pac_net_segments;

		procedure query_segments (segment_cursor : in pac_net_segments.cursor) is
			use pac_device_ports;

			use et_netlists;
			use pac_netchanger_ports;			
			use pac_submodule_ports;
			
			-- Inserts the device/port in result.devices. Skips the device/port
			-- according to the given assembly variant.
			procedure query_devices (device_cursor : in pac_device_ports.cursor) is begin
				if et_assembly_variants.is_mounted (
					device		=> element (device_cursor).device_name, -- IC4, R101
					variant		=> variant) 
				then
					--put_line (to_string (element (device_cursor)));
					
					insert (
						container	=> result.devices,
						new_item	=> element (device_cursor));
				end if;

				exception
					when event: others =>
						raise constraint_error with to_string (element (device_cursor))
						--put_line (to_string (element (device_cursor))
						& " already in set !";
						
			end query_devices;

			
		begin
			-- Collect device ports of segment according to given assembly variant.
			iterate (element (segment_cursor).ports.devices, query_devices'access);

			-- Ports of netchangers and submodules go into the result right away
			-- because they are not affected by any assembly variants.
			union (result.netchangers, element (segment_cursor).ports.netchangers);
			union (result.submodules, element (segment_cursor).ports.submodules);
		end query_segments;

		
		procedure query_strands (strand_cursor : in pac_strands.cursor) is begin
			iterate (element (strand_cursor).segments, query_segments'access);
		end query_strands;

		
	begin
		--put_line ("net " & to_string (key (net)));		
		iterate (element (net).strands, query_strands'access);
		return result;
	end get_ports;




	procedure iterate (
		devices	: in pac_devices_sch.map;
		process	: not null access procedure (position : in pac_devices_sch.cursor);
		proceed	: not null access boolean)
	is 
		use pac_devices_sch;
		c : pac_devices_sch.cursor := devices.first;
	begin
		while c /= no_element and proceed.all = TRUE loop
			process (c);
			next (c);
		end loop;
	end iterate;
	

	

	function to_string (
		device_cursor : in pac_devices_sch.cursor)
		return string
	is begin
		return to_string (key (device_cursor));
	end to_string;



	function get_device_model (
		device : in pac_devices_sch.cursor)
		return pac_devices_lib.cursor
	is
		use et_device_model_names;
		model_file : pac_device_model_file.bounded_string;
	begin
		-- The name of the device model file is THE link
		-- from device in schematic to device in library:
		model_file := pac_devices_sch.element (device).model;
		return locate_device (model_file);
	end get_device_model;


	

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
		device_cursor_lib := locate_device (device_model);
		
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
			locate_device (pac_devices_sch.element (device).model);

		-- This is the package variant used by the given device:
		variant_sch : constant pac_package_variant_name.bounded_string :=
			pac_devices_sch.element (device).variant; -- N, D


		use et_device_model;
		use et_device_model_names;

		
		procedure query_model (
			model	: in pac_device_model_file.bounded_string;
			device	: in type_device_model)
		is
			use pac_variants;

			-- Locate the package variant of the given device
			-- in the device model:
			variant_lib : constant pac_variants.cursor := 
				find (device.variants, variant_sch);

			
			procedure query_terminal_port_map (
				name	: in pac_package_variant_name.bounded_string;
				variant	: in type_variant)
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
		use pac_variants;
		variant_lib : constant pac_variants.cursor := 
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


	
	procedure device_name_in_use (
		name	: in type_device_name;	-- IC1, MH1, ...
		by_cat	: in type_device_category)	-- electrical/non-electrical
	is begin
		case by_cat is
			when NON_ELECTRICAL =>
				log (ERROR, "Name " & enclose_in_quotes (to_string (name)) &
					 " already used by another non-electrical device !",
					 console => true);

			when ELECTRICAL =>
				log (ERROR, "Name " & enclose_in_quotes (to_string (name)) &
					 " already used by another electrical device !",
					 console => true);
		end case;
		
		raise constraint_error;
	end device_name_in_use;

	

	
end et_schematic;
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
