------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                          PACKAGE VARIANT                                 --
--                                                                          --
--                              S p e c                                     --
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


with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded; 		use ada.strings.bounded;

with ada.containers; 			use ada.containers;
with ada.containers.ordered_maps;

with et_package_name;			use et_package_name;
with et_package_model_name;		use et_package_model_name;
with et_package_variant_name;	use et_package_variant_name;
with et_terminal_name;			use et_terminal_name;
with et_port_names;				use et_port_names;
with et_unit_name;				use et_unit_name;
with et_package_variant_terminal_port_map;	use et_package_variant_terminal_port_map;


package et_package_variant is


	
	type type_package_variant is record
		package_model		: pac_package_model_file.bounded_string; -- libraries/packages/smd/SOT23.pac
		terminal_port_map	: pac_terminal_port_map.map; -- which port is connected with with terminal
	end record;


	use pac_package_variant_name;
	
	package pac_package_variants is new ordered_maps (
		key_type 		=> pac_package_variant_name.bounded_string, -- D, N
		element_type 	=> type_package_variant);

	use pac_package_variants;

	
	-- Returns the name of the first package variant
	-- of the given list:
	function get_first_package_variant (
		variants : in pac_package_variants.map)
		return pac_package_variant_name.bounded_string;

								   
	-- Returns the number of package variants that
	-- the given list contains:
	function get_variant_count (
		variants : in pac_package_variants.map)
		return natural;
	

	-- For enquiries of port and unit that is linked to a terminal these
	-- types are required to indicate whether a terminal is linked to a port at all.
	-- A terminal can be left unconnected (NC in datasheets):
	type type_terminal_linked is new boolean;
	
	type type_get_port_result (
		linked : type_terminal_linked := false)
	is record
		case linked is
			when TRUE =>
				unit	: pac_unit_name.bounded_string; -- A, B, GPIO1, ...
				port	: pac_port_name.bounded_string; -- IN1, IN2, ...
			when FALSE => null;
		end case;
	end record;
	

	-- Returns the unit and port that is linked to the given
	-- terminal. If no unit and port found, then an unconnected
	-- terminal is returned:
	function get_unit_and_port (
		variant		: in pac_package_variants.cursor;
		terminal	: in pac_terminal_name.bounded_string)
		return type_get_port_result;

	
	
	-- This is basically the reverse of get_unit_and_port.
	-- Returns the name of the terminal that is linked to the given
	-- unit and port. If no terminal found, then an exception is raised:
	function get_terminal (
		variant	: in pac_package_variants.cursor;
		unit	: in pac_unit_name.bounded_string;
		port	: in pac_port_name.bounded_string)
		return pac_terminal_name.bounded_string;



	type type_terminal is record
		name	: pac_terminal_name.bounded_string; -- H7
		unit	: pac_unit_name.bounded_string; -- IO-BANK1
		port	: pac_port_name.bounded_string; -- GPIO3
	end record;


	
	-- Returns the given terminal as string. 
	-- If show_unit is true, the unit name is output.
	-- If preamble is true, each property of the terminal is headed by a short preamble.
	function to_string (
		terminal	: in type_terminal;
		show_unit	: in boolean := false;
		preamble	: in boolean := true)
		return string;

	
	
end et_package_variant;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
