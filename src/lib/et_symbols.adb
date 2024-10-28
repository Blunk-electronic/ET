------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                              SYMBOLS                                     --
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
with ada.characters.handling;	use ada.characters.handling;

with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.exceptions; 			use ada.exceptions;
with et_alignment;				use et_alignment;


package body et_symbols is

	
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
		log (text => to_string (text.alignment),
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
		rotation	: in et_coordinates_2.type_rotation_model)
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
		phs.name.rotation := snap (to_rotation (phs.name.rotation) + rotation);

		phs.value.rotation := snap (to_rotation (phs.value.rotation) + rotation);

		phs.purpose.rotation := snap (to_rotation (phs.purpose.rotation) + rotation);
	end rotate;

	
	-- Use this function to adopt placeholder position and rotation of a symbol.
	-- Rotates the positions of placeholders and their rotation about
	-- their own origin according to rotation given by destination:
	function rotate_placeholders (
		symbol_cursor	: in pac_symbols.cursor;
		destination		: in et_coordinates_2.type_position)
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
