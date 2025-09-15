------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                          PACKAGE VARIANT                                 --
--                                                                          --
--                              S p e c                                     --
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

with et_package_names;			use et_package_names;
with et_terminals;				use et_terminals;
with et_port_names;				use et_port_names;
with et_unit_name;				use et_unit_name;


package et_package_variant is


	-- The variant is usually a suffix in a device value, given by its manufacturer. The variant is a manufacturer
	-- specific abbrevation for the package a device comes with.
	-- Example: An opamp made by TI can be the type TL084N or TL084D. N means the NDIP14 package
	-- whereas D means the SO14 package.
	-- If a device has package variants, a suffix after the value indicates the package
	-- The variant name is manufacturer specific. example: TL084D or TL084N
	-- device package variant names like "N" or "D" are stored in bounded strings.
	variant_name_characters : character_set := 
		to_set (ranges => (('A','Z'),('a','z'),('0','9'))) or to_set ("_-");
	
	variant_name_length_max : constant positive := 50;
	package pac_package_variant_name is new generic_bounded_length (variant_name_length_max);
	use pac_package_variant_name;

	-- function to_string (package_variant : in pac_package_variant_name.bounded_string) return string;
	-- converts a pac_package_variant_name to a string.
	
	function to_variant_name (variant_name : in string) return pac_package_variant_name.bounded_string;

	procedure check_variant_name_length (variant_name : in string);
	-- tests if the given variant name is not longer than allowed
	
	procedure check_variant_name_characters (
		variant		: in pac_package_variant_name.bounded_string;
		characters	: in character_set := variant_name_characters);
	-- Tests if the given variant name contains only valid characters as specified
	-- by given character set.
	-- Raises exception if invalid character found.

	
	type type_port_in_terminal_port_map is record
		name	: pac_port_name.bounded_string; -- CLK, CE, VSS -- CS rename to port
		unit	: pac_unit_name.bounded_string; -- GPIO_BANK_3
	end record;
	
	package pac_terminal_port_map is new ordered_maps (
		key_type 		=> et_terminals.pac_terminal_name.bounded_string, -- H7, 14
		"<"				=> et_terminals.pac_terminal_name."<",
		element_type 	=> type_port_in_terminal_port_map); -- unit A, OE1


	
	type type_variant is record -- CS rename to type_package_variant
		package_model		: pac_package_model_file_name.bounded_string; -- libraries/packages/smd/SOT23.pac
		terminal_port_map	: pac_terminal_port_map.map; -- which port is connected with with terminal
	end record;

	
	package pac_variants is new ordered_maps ( -- CS rename to pac_package_variants
		key_type 		=> pac_package_variant_name.bounded_string, -- D, N
		element_type 	=> type_variant);

	use pac_variants;

	
	-- Returns the name of the first package variant
	-- of the given list:
	function get_first_variant (
		variants : in pac_variants.map)
		return pac_package_variant_name.bounded_string;

								   
	-- Returns the number of package variants that
	-- the given list contains:
	function get_variant_count (
		variants : in pac_variants.map)
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
		variant		: in pac_variants.cursor;
		terminal	: in pac_terminal_name.bounded_string)
		return type_get_port_result;

	
	
	-- This is basically the reverse of get_unit_and_port.
	-- Returns the name of the terminal that is linked to the given
	-- unit and port. If no terminal found, then an exception is raised:
	function get_terminal (
		variant	: in pac_variants.cursor;
		unit	: in pac_unit_name.bounded_string;
		port	: in pac_port_name.bounded_string)
		return pac_terminal_name.bounded_string;



	type type_terminal is record
		name	: et_terminals.pac_terminal_name.bounded_string; -- H7
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
