------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                              SYMBOLS                                     --
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

with ada.strings; 				use ada.strings;
--with ada.strings.maps;
with ada.strings.maps.constants;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.strings.unbounded; 	use ada.strings.unbounded;
with ada.numerics.real_arrays;  use ada.numerics.real_arrays;
with ada.directories;			use ada.directories;
with ada.exceptions; 			use ada.exceptions;

-- with et_import;

package body et_symbols is

-- 	function to_string (style : in type_text_style) return string is begin
-- 		return to_lower (type_text_style'image (style));
-- 	end;
-- 	
-- 	function to_text_style (style : in string) return type_text_style is begin
-- 		return type_text_style'value (style);
-- 	end;



	function to_string (meaning : in type_placeholder_meaning) return string is begin
		return to_lower (type_placeholder_meaning'image (meaning));
	end;

	function to_meaning (meaning : in string) return type_placeholder_meaning is begin
		return type_placeholder_meaning'value (meaning);
	end;





	procedure write_placeholder_properties (
		placeholder		: in type_text_placeholder;
		log_threshold	: in type_log_level) 
	is begin
		-- meaning
		log (text => to_string (placeholder.meaning), level => log_threshold);
		log_indentation_up;
		
		-- position
		log (text => to_string (placeholder.position), level => log_threshold);

		-- size
		log (text => to_string (placeholder.size), level => log_threshold);

		-- rotation
		log (text => to_string (placeholder.rotation), level => log_threshold); 

		-- visible
		--log (text => "visible "
		--	& to_lower (et_libraries.type_text_visible'image (placeholder.visible)), level => log_threshold);

		-- alignment
		log (text => et_text.to_string (placeholder.alignment),
			level => log_threshold);

		log_indentation_down;
	end write_placeholder_properties;

	
	procedure write_text_properies (
		text 			: in type_text;
		log_threshold	: in type_log_level) 
	is
		use et_text;
	begin
		log_indentation_up;
		
		-- content
		if pac_text_content.length (text.content) > 0 then
			log (text => "content '" & et_text.to_string (text.content) & "'",
				level => log_threshold);
		else
			log (text => "no content", level => log_threshold);
		end if;

		-- position
		log (text => to_string (text.position), level => log_threshold + 1);
		
		-- size
		log (text => "size" & to_string (text.size), level => log_threshold + 1);

		-- style
-- 		log (text => "style " & to_lower (type_text_style'image (text.style)),
-- 			 level => log_threshold + 1);

		-- rotation
		log (text => to_string (text.rotation), level => log_threshold + 1);

		-- visible
		--log (text => "visible " & to_lower(et_libraries.type_text_visible'image (text.visible)),
		--	level => log_threshold + 1);

		-- alignment
		log (text => et_text.to_string (text.alignment),
			level => log_threshold + 1);
				
-- 		log_indentation_down;
		log_indentation_down;
	end write_text_properies;


	function content (text : in type_text) return string is
	-- Returns the content of the given text as string.
		c : et_text.pac_text_content.bounded_string;
	begin
		c := text.content;
		return et_text.to_string (c);
	end;


	

	
	
	function to_string (direction : in type_port_direction) return string is begin
		return to_lower (type_port_direction'image (direction));
	end;
	
	function to_port_direction (direction : in string) return type_port_direction is begin
		return type_port_direction'value (direction);
	end;




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

	

	function to_string (port : in pac_port_name.bounded_string) return string is begin
		return pac_port_name.to_string (port);
	end;

	function to_port_name (name : in string) return pac_port_name.bounded_string is begin
		return pac_port_name.to_bounded_string (name);
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



	function to_string (
		appearance	: in type_appearance;
		verbose		: in boolean := false)
		return string is
	-- Returns the given component appearance as string.
	begin
		if verbose then
			case appearance is
				when VIRTUAL =>
					return ("appears in schematic only (virtual device)");
				when PCB =>
					return ("appears in schematic and layout");
			end case;
		else
			return to_lower (type_appearance'image (appearance));
		end if;
	end;

	function to_appearance (appearance : in string) return type_appearance is begin
		return type_appearance'value (appearance);
	end;	





	function to_string (filled : in type_circle_filled) return string is begin
		return to_lower (type_circle_filled'image (filled));
	end;

	function to_circle_filled (filled : in string) return type_circle_filled is begin
		return type_circle_filled'value (filled);
	end;





	function to_string (name : in pac_symbol_model_file.bounded_string) 
		return string is
	begin
		return pac_symbol_model_file.to_string (name);
	end to_string;

	function to_file_name (name : in string)
		return pac_symbol_model_file.bounded_string is
	begin
		return pac_symbol_model_file.to_bounded_string (name);
	end;






	function locate (symbol : in pac_symbol_model_file.bounded_string) -- ../libraries/symbols/NAND.sym
		return pac_symbols.cursor is
	begin
		return pac_symbols.find (symbols, symbol);
	end locate;

	function is_real (symbol : in pac_symbols.cursor)
		return boolean
	is begin
		case element (symbol).appearance is
			when PCB		=> return true;
			when VIRTUAL	=> return false;
		end case;
	end is_real;

	
	
	procedure rotate (
		phs			: in out type_rotated_placeholders;
		rotation	: in type_rotation)
	is begin
		-- Rotate the POSITIONS	of the placeholders about
		-- the origin of the symbol:
		rotate_by (phs.name.position, rotation);
		rotate_by (phs.value.position, rotation);
		rotate_by (phs.purpose.position, rotation);

		-- Rotate the placeholders about THEIR OWN ORIGIN.
		-- The resulting angle is the sum of the initial 
		-- rotation (given by the symbol model) and the rotation
		-- of the unit.
		-- After summing up the rotation must be snapped to either
		-- HORIZONTAL or VERTICAL so that the text is readable
		-- from the right or from the front of the drawing.
		phs.name.rotation := pac_text.snap (
			pac_text.to_rotation (phs.name.rotation) + rotation);

		phs.value.rotation := pac_text.snap (
			pac_text.to_rotation (phs.value.rotation) + rotation);

		phs.purpose.rotation := pac_text.snap (
			pac_text.to_rotation (phs.purpose.rotation) + rotation);

	end rotate;

	-- Use this function to adopt placeholder position and rotation of a symbol.
	-- Rotates the positions of placeholders and their rotation about
	-- their own origin according to rotation given by destination:
	function rotate_placeholders (
		symbol_cursor	: in pac_symbols.cursor;
		destination		: in et_coordinates.type_position)
		return type_rotated_placeholders
	is
		use pac_symbols;
		r : type_rotated_placeholders; -- to be returned
	begin
		r.name		:= element (symbol_cursor).name;
		r.value		:= element (symbol_cursor).value;
		r.purpose	:= element (symbol_cursor).purpose;

		-- rotate the positions of placeholders according to rotation given by caller:
		rotate (r, get_rotation (destination));
		
		return r;
	end rotate_placeholders;
	
end et_symbols;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
