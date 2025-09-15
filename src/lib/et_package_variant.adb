------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                          PACKAGE VARIANT                                 --
--                                                                          --
--                              B o d y                                     --
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

with ada.text_io;				use ada.text_io;

with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;
with ada.strings; 				use ada.strings;

with et_logging;				use et_logging;
with et_exceptions;				use et_exceptions;


package body et_package_variant is

	
	-- function to_string (package_variant : in pac_package_variant_name.bounded_string) return string is begin
	-- 	return pac_package_variant_name.to_string (package_variant);
	-- end;


	
	function to_variant_name (variant_name : in string) 
		return pac_package_variant_name.bounded_string
	is begin
		return pac_package_variant_name.to_bounded_string (variant_name);
	end;


	
	procedure check_variant_name_length (variant_name : in string) is
	-- tests if the given variant name is not longer than allowed
	begin
		if variant_name'length > variant_name_length_max then
			log (WARNING, "variant name too long. Max. length is" 
				 & positive'image (variant_name_length_max) & " !");
		end if;
	end check_variant_name_length;


	
	procedure check_variant_name_characters (
		variant		: in pac_package_variant_name.bounded_string;
		characters	: in character_set := variant_name_characters) 
	is
		invalid_character_position : natural := 0;
	begin
		-- Test given variant name and get position of possible invalid characters.
		invalid_character_position := index (
			source => variant,
			set => characters,
			test => outside);

		-- Evaluate position of invalid character.
		if invalid_character_position > 0 then
			log (WARNING, "invalid character in variant name " 
				& to_string (variant) & " at position" & natural'image (invalid_character_position));
		end if;
	end check_variant_name_characters;





	function get_first_variant (
		variants : in pac_variants.map)
		return pac_package_variant_name.bounded_string
	is begin
		return key (variants.first);
	end;


	

	function get_variant_count (
		variants : in pac_variants.map)
		return natural
	is begin
		return natural (variants.length);
	end;


	


	function get_unit_and_port (
		variant		: in pac_variants.cursor;
		terminal	: in pac_terminal_name.bounded_string)
		return type_get_port_result
	is
		result : type_get_port_result;

		use pac_variants;
		
		procedure query_terminal_port_map (
			name	: in pac_package_variant_name.bounded_string;
			variant	: in type_variant)
		is
			-- Locate in the given package variant the given terminal:
			use pac_terminal_port_map;
			c : pac_terminal_port_map.cursor := 
				find (variant.terminal_port_map, terminal);
		begin
			if c /= pac_terminal_port_map.no_element then -- terminal exists
				result := (
					linked	=> TRUE,
					unit	=> element (c).unit,
					port	=> element (c).name);
			else
				-- If the terminal can not be found in the map then
				-- it is not linked to any port:
				result := (linked => FALSE);
			end if;
		end query_terminal_port_map;
								
	begin
		query_element (variant, query_terminal_port_map'access);
		return result;
	end get_unit_and_port;

	


	function get_terminal (
		variant	: in pac_variants.cursor;
		unit	: in pac_unit_name.bounded_string;
		port	: in pac_port_name.bounded_string)
		return pac_terminal_name.bounded_string
	is
		use pac_terminal_name;
		result : pac_terminal_name.bounded_string;

		use pac_variants;
		
		procedure query_terminal_port_map (
			name	: in pac_package_variant_name.bounded_string;
			variant	: in type_variant)
		is
			use pac_unit_name;
			use pac_port_name;
			use pac_terminal_port_map;
			c : pac_terminal_port_map.cursor := variant.terminal_port_map.first;
		begin
			while c /= pac_terminal_port_map.no_element loop
				if element (c).unit = unit and then element (c).name = port then 
					result := key (c);
					exit;
				end if;
				next (c);
			end loop;
		end query_terminal_port_map;
								
	begin
		query_element (variant, query_terminal_port_map'access);

		-- Raise exception if no terminal has been found:
		if length (result) = 0 then
			raise semantic_error_1 with "No terminal found !";
			-- CS output variant name, unit and port
		end if;
		
		return result;
	end get_terminal;





	function to_string (
		terminal	: in type_terminal;
		show_unit	: in boolean := false;
		preamble	: in boolean := true)
		return string 
	is
		use pac_unit_name;
	begin
		case preamble is
			when true =>
				case show_unit is
					when true =>
						return (" port " & to_string (port => terminal.port) 
							& " unit " & to_string (terminal.unit)
							& " terminal " & to_string (terminal.name)
							& latin_1.space);
						
					when false =>
						return (" port " & to_string (port => terminal.port) 
							& " terminal " & to_string (terminal.name)
							& latin_1.space);
				end case;
						
			when false =>
				case show_unit is
					when true =>
						return (latin_1.space & to_string (port => terminal.port) 
							& latin_1.space & to_string (terminal.unit)
							& latin_1.space & to_string (terminal.name)
							& latin_1.space);
						
					when false =>
						return (latin_1.space & to_string (port => terminal.port) 
							& latin_1.space & to_string (terminal.name)
							& latin_1.space);
				end case;
		end case;
		
	end to_string;

	
		
end et_package_variant;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
