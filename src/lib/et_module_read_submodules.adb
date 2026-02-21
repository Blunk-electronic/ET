------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      MODULE READ / SUBMODULES                            --
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
--                                                                          --
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
-- ToDo:
-- - clean up
--
--
--

with ada.text_io;					use ada.text_io;
with ada.characters;				use ada.characters;
with ada.strings;					use ada.strings;

with et_module;						use et_module;
with et_module_names;				use et_module_names;
with et_module_instance;			use et_module_instance;
with et_keywords;					use et_keywords;

with et_schematic_geometry;
with et_board_geometry;

with et_schematic_coordinates;

with et_netchangers.schematic;		use et_netchangers.schematic;

with et_submodules;					use et_submodules;
with et_net_names;					use et_net_names;




package body et_module_read_submodules is

	use pac_generic_modules;


	submodule_port_name	: pac_net_name.bounded_string; -- RESET
	submodule_ports		: et_submodules.pac_submodule_ports.map;
	submodule_name 		: et_module_instance.pac_module_instance_name.bounded_string; -- MOT_DRV_3
	submodule_port 		: et_submodules.type_submodule_port;
	submodule 			: et_submodules.type_submodule;

	
	

		
	function to_size (
		line : in type_fields_of_line; -- "size x 30 y 40"
		from : in type_field_count_positive)
		return et_submodules.type_submodule_size 
	is
		use et_schematic_geometry.pac_geometry_2;
		
		size : type_submodule_size; -- to be returned
		place : type_field_count_positive := from; -- the field being read from given line

		-- CS: flags to detect missing x or y
	begin
		while place <= get_field_count (line) loop

			-- We expect after the x the corresponding value for x
			if f (line, place) = keyword_x then
				size.x := to_distance (f (line, place + 1));

			-- We expect after the y the corresponding value for y
			elsif f (line, place) = keyword_y then
				size.y := to_distance (f (line, place + 1));

			else
				invalid_keyword (f (line, place));
			end if;
				
			place := place + 2;
		end loop;
		
		return size;
	end to_size;


	
	
	
	
	procedure read_submodule (
		line : in type_fields_of_line)
	is
		kw : constant string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_file then -- file $ET_TEMPLATES/motor_driver.mod
			expect_field_count (line, 2);
			submodule.file := et_submodules.to_submodule_path (f (line, 2));

		elsif kw = keyword_name then -- name stepper_driver
			expect_field_count (line, 2);
			submodule_name := to_instance_name (f (line, 2));

		elsif kw = keyword_position then -- position sheet 3 x 130 y 210
			expect_field_count (line, 7);

			-- extract position of submodule starting at field 2
			submodule.position := et_schematic_coordinates.to_position (line, 2);

		elsif kw = keyword_size then -- size x 30 y 30
			expect_field_count (line, 5);

			-- extract size of submodule starting at field 2
			submodule.size := to_size (line, 2);

		elsif kw = keyword_position_in_board then -- position_in_board x 23 y 0.2 rotation 90.0
			expect_field_count (line, 7);

			-- extract position of submodule starting at field 2
			submodule.position_in_board := et_board_geometry.pac_geometry_2.to_position (line, 2);

		elsif kw = keyword_view_mode then -- view_mode origin/instance
			expect_field_count (line, 2);
			submodule.view_mode := et_submodules.to_view_mode (f (line, 2));

		else
			invalid_keyword (kw);
		end if;
	end read_submodule;


	
	
	
	
	procedure read_submodule_port (
		line : in type_fields_of_line)
	is
		use et_schematic_geometry.pac_geometry_2;
		kw : constant string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_name then -- name clk_out
			expect_field_count (line, 2);
			submodule_port_name := to_net_name (f (line, 2));

		elsif kw = keyword_position then -- position x 0 y 10
			expect_field_count (line, 5);

			-- extract port position starting at field 2
			submodule_port.position := to_vector_model (line, 2);

		elsif kw = keyword_direction then -- direction master/slave
			expect_field_count (line, 2);

			submodule_port.direction := to_port_name (f (line, 2));
		else
			invalid_keyword (kw);
		end if;
	end read_submodule_port;


	

	
	
	procedure insert_submodule_port (
		line : in type_fields_of_line)
	is
		cursor : et_submodules.pac_submodule_ports.cursor;
		inserted : boolean;

		use pac_net_name;
	begin
		-- Test whether the port sits at the edge of the submodule box:
		if et_submodules.at_edge (submodule_port.position, submodule.size) then
			
			-- append port to collection of submodule ports
			et_submodules.pac_submodule_ports.insert (
				container	=> submodule_ports,
				key			=> submodule_port_name, -- RESET
				new_item	=> submodule_port,
				inserted	=> inserted,
				position	=> cursor
				);

			if not inserted then
				log (ERROR, "port " & 
					to_string (submodule_port_name) & " already used !",
					console => true
					);
				raise constraint_error;
			end if;

		else
			log (ERROR, "port " & to_string (submodule_port_name)
				& " is not on the edge of the submodule !");
			
			raise constraint_error;
		end if;

		-- clean up for next port
		submodule_port_name := to_net_name ("");
		submodule_port := (others => <>);
		
	end insert_submodule_port;


	
	
	
	-- copy collection of ports to submodule
	procedure assign_submodule_ports is	begin
		submodule.ports := submodule_ports;

		-- clean up for next collection of ports
		et_submodules.pac_submodule_ports.clear (submodule_ports);
	end;
	
	

	

	procedure insert_submodule (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is
	
		procedure insert_submodule (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			inserted : boolean;
			use et_submodules.pac_submodules;
			cursor : pac_submodules.cursor;
		begin
			log (text => "submodule " & to_string (submodule_name), level => log_threshold + 1);

			-- CS: notify about missing parameters (by reading the parameter-found-flags)
			-- If a parameter is missing, the default is assumed. See type_submodule spec.
			
			pac_submodules.insert (
				container	=> module.submods,
				key			=> submodule_name,	-- the instance name like MOT_DRV_3
				new_item	=> submodule,
				inserted	=> inserted,
				position	=> cursor);

			if not inserted then
				log (ERROR, "submodule '" & to_string (submodule_name) 
					& "' already exists !", console => true);
				raise constraint_error;
			end if;

			-- The submodule/template (kept in submodule.file) will be read later once the 
			-- parent module has been read completely.
			
			-- clean up for next submodule
			submodule_name := to_instance_name ("");
			submodule := (others => <>);			
		end insert_submodule;
	
	
	begin
		log (text => "module " & to_string (module_cursor)
			& " insert submodule",
		level => log_threshold);
		
		log_indentation_up;
		
		generic_modules.update_element (module_cursor, insert_submodule'access);
		log_indentation_down;		
	end insert_submodule;
	
	
		
	
	
end et_module_read_submodules;

	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
