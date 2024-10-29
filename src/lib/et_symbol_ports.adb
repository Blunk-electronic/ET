------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                            SYMBOL PORTS                                  --
--                                                                          --
--                              B o d y                                     --
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
-- with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;

with ada.strings.fixed; 		use ada.strings.fixed;



package body et_symbol_ports is



	function to_string (visible : in type_port_name_visible) return string is begin
		return to_lower (type_port_name_visible'image (visible));
	end;

	function to_port_name_visible (visible : in string) return type_port_name_visible is begin
		return type_port_name_visible'value (visible);
	end;




	function to_string (visible : in type_terminal_name_visible) return string is begin
		return to_lower (type_terminal_name_visible'image (visible));
	end;

	function to_terminal_name_visible (visible : in string) return type_terminal_name_visible is begin
		return type_terminal_name_visible'value (visible);
	end;

	




	function to_string (sensitivity : in type_sensitivity_edge) return string is begin
		return to_lower (type_sensitivity_edge'image (sensitivity));
	end;

	function to_sensitivity_edge (sensitivity : in string) return type_sensitivity_edge is begin
		return type_sensitivity_edge'value (sensitivity);
	end;

	

	function to_string (sensitivity : in type_sensitivity_level) return string is begin
		return to_lower (type_sensitivity_level'image (sensitivity));
	end;

	function to_sensitivity_level (sensitivity : in string) return type_sensitivity_level is begin
		return type_sensitivity_level'value (sensitivity);
	end;




	function to_string (inverted : in type_output_inverted) return string is begin
		return to_lower (type_output_inverted'image (inverted));
	end;

	function to_output_inverted (inverted : in string) return type_output_inverted is begin
		return type_output_inverted'value (inverted);
	end;



	function to_string (weakness : in type_output_weakness) return string is begin
		return to_lower (type_output_weakness'image (weakness));
	end;

	function to_output_weakness (weakness : in string) return type_output_weakness is begin
		return type_output_weakness'value (weakness);
	end;



	function to_string (tristate : in type_output_tristate) return string is begin
		return to_lower (type_output_tristate'image (tristate));
	end;

	function to_output_tristate (tristate : in string) return type_output_tristate is begin
		return type_output_tristate'value (tristate);
	end;





	function to_string (level : in type_power_level) return string is
	-- Converts the power level (like LEVEL_POSITIVE) to a string (like positive).
	-- The prefix LEVEL_ is removed.
		level_string : string := to_lower (type_power_level'image (level)); -- level_positive, level_negative
		A : positive := index (level_string, "_") + 1; -- the position after the first underscore
		B : positive := level_string'length;
	begin
		return level_string (A .. B);
	end;

	function to_power_level (level : in string) return type_power_level is 
	-- Converts the power level (like positive) to power level (like LEVEL_POSITIVE).
	-- The prefix LEVEL_ is prepended.
	begin
		return type_power_level'value ("LEVEL_" & level);
	end;



	
	
end et_symbol_ports;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
