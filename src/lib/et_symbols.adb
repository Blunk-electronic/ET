------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                              SYMBOLS                                     --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2019 Mario Blunk, Blunk electronic                 --
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

with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;
with ada.text_io;				use ada.text_io;
with ada.strings; 				use ada.strings;
--with ada.strings.maps;
with ada.strings.maps.constants;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.strings.unbounded; 	use ada.strings.unbounded;
with ada.numerics.real_arrays;  use ada.numerics.real_arrays;
with ada.directories;			use ada.directories;
with ada.exceptions; 			use ada.exceptions;

with et_string_processing;
with et_coordinates;
with et_import;
with et_text;

package body et_symbols is
	
	use geometry;
	
	
	function to_text_size (size : in type_distance) return pac_text.type_text_size is
	-- Converts given distance to type_text_size. Raises error on excessive text size.
		use et_string_processing;

		function to_string (
			size		: in pac_text.type_text_size;
			preamble	: in boolean := true) return string is
		-- Returns the given text size as string.
		begin
			if preamble then
				return "size " & geometry.to_string (size);
			else
				return geometry.to_string (size);
			end if;
		end to_string;

	begin
		if size not in pac_text.type_text_size then
			log (ERROR, "text " 
				 & to_string (size => size, preamble => true)  
				 & " out of range !",
				 console => true);

			log (text => "Allowed range is " & to_string (pac_text.type_text_size'first, preamble => false) & " .. "
				 & to_string (pac_text.type_text_size'last, preamble => false),
				 console => true);

			raise constraint_error;
		end if;
		return size;
	end to_text_size;


	function to_string (style : in type_text_style) return string is begin
		return to_lower (type_text_style'image (style));
	end to_string;
	
	function to_text_style (style : in string) return type_text_style is begin
		return type_text_style'value (style);
	end to_text_style;



	function to_string (meaning : in type_text_meaning) return string is begin
		return to_lower (type_text_meaning'image (meaning));
	end to_string;

	function to_text_meaning (meaning : in string) return type_text_meaning is begin
		return type_text_meaning'value (meaning);
	end to_text_meaning;



	function to_component_attribute_text_size (text : in string) return type_placeholder_text_size is
	-- Converts a string to a type_placeholder_text_size.
	begin
		return type_placeholder_text_size'value (text);
	end to_component_attribute_text_size;



	procedure write_placeholder_properties (
	-- Writes the properties of the given placeholder.
		placeholder		: in type_text_placeholder;
		log_threshold	: in et_string_processing.type_log_level) is

		use et_string_processing;
	begin
		-- meaning
		log (text => to_string (placeholder.meaning), level => log_threshold);
		log_indentation_up;
		
		-- position
		log (text => to_string (placeholder.position), level => log_threshold);

		-- size
		log (text => to_string (placeholder.size), level => log_threshold);

		-- style
		log (text => "style "
			& to_lower (type_text_style'image (placeholder.style)), level => log_threshold);

		-- line width
		log (text => "line width"
			& to_string (placeholder.line_width), level => log_threshold);

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
	-- Outputs the properties of the given text.
		text 			: in type_text;
		log_threshold	: in et_string_processing.type_log_level) is

		use et_string_processing;
		use et_text;
	begin
		log_indentation_up;
		
		-- content
		if type_text_content.length (text.content) > 0 then
			log (text => "content '" & et_text.to_string (text.content) & "'",
				level => log_threshold);
		else
			log (text => "no content", level => log_threshold);
		end if;

		-- position
		log (text => to_string (text.position), level => log_threshold + 1);
		
		-- size
		log (text => "size" & geometry.to_string (text.size), level => log_threshold + 1);

		-- style
		log (text => "style " & to_lower (type_text_style'image (text.style)),
			 level => log_threshold + 1);

		-- line width
		log (text => "line width" & geometry.to_string (text.line_width),
			level => log_threshold + 1);

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
		c : et_text.type_text_content.bounded_string;
	begin
		c := text.content;
		return et_text.to_string (c);
	end content;


	

	function to_terminal_name_text_size (text : in string) return type_terminal_name_text_size is begin
		return type_terminal_name_text_size'value (text);
	end to_terminal_name_text_size;


	
	
	function to_string (direction : in type_port_direction) return string is begin
		return latin_1.space & to_lower (type_port_direction'image (direction));
	end to_string;
	
	function to_port_direction (direction : in string) return type_port_direction is begin
		return type_port_direction'value (direction);
	end to_port_direction;




	function to_string (visible : in type_port_name_visible) return string is begin
		return latin_1.space & to_lower (type_port_name_visible'image (visible));
	end to_string;

	function to_port_name_visible (visible : in string) return type_port_name_visible is begin
		return type_port_name_visible'value (visible);
	end to_port_name_visible;




	function to_string (visible : in type_terminal_name_visible) return string is begin
		return latin_1.space & to_lower (type_terminal_name_visible'image (visible));
	end to_string;

	function to_terminal_name_visible (visible : in string) return type_terminal_name_visible is begin
		return type_terminal_name_visible'value (visible);
	end to_terminal_name_visible;

	

	function to_string (port : in type_port_name.bounded_string) return string is begin
		return type_port_name.to_string (port);
	end to_string;

	function to_port_name (name : in string) return type_port_name.bounded_string is begin
		return type_port_name.to_bounded_string (name);
	end to_port_name;



	
	function to_port_name_text_size (text : in string) return type_port_name_text_size is begin
		return type_port_name_text_size'value (text);
	end;



	function to_string (sensitivity : in type_sensitivity_edge) return string is begin
		return latin_1.space & to_lower (type_sensitivity_edge'image (sensitivity));
	end to_string;

	function to_sensitivity_edge (sensitivity : in string) return type_sensitivity_edge is begin
		return type_sensitivity_edge'value (sensitivity);
	end to_sensitivity_edge;

	

	function to_string (sensitivity : in type_sensitivity_level) return string is begin
		return latin_1.space & to_lower (type_sensitivity_level'image (sensitivity));
	end to_string;

	function to_sensitivity_level (sensitivity : in string) return type_sensitivity_level is begin
		return type_sensitivity_level'value (sensitivity);
	end to_sensitivity_level;




	function to_string (inverted : in type_output_inverted) return string is begin
		return latin_1.space & to_lower (type_output_inverted'image (inverted));
	end to_string;

	function to_output_inverted (inverted : in string) return type_output_inverted is begin
		return type_output_inverted'value (inverted);
	end to_output_inverted;



	function to_string (weakness : in type_output_weakness) return string is begin
		return latin_1.space & to_lower (type_output_weakness'image (weakness));
	end to_string;

	function to_output_weakness (weakness : in string) return type_output_weakness is begin
		return type_output_weakness'value (weakness);
	end to_output_weakness;



	function to_string (tristate : in type_output_tristate) return string is begin
		return latin_1.space & to_lower (type_output_tristate'image (tristate));
	end to_string;

	function to_output_tristate (tristate : in string) return type_output_tristate is begin
		return type_output_tristate'value (tristate);
	end to_output_tristate;





	function to_string (level : in type_power_level) return string is
	-- Converts the power level (like LEVEL_POSITIVE) to a string (like positive).
	-- The prefix LEVEL_ is removed.
		level_string : string := to_lower (type_power_level'image (level)); -- level_positive, level_negative
		A : positive := index (level_string, "_") + 1; -- the position after the first underscore
		B : positive := level_string'length;
	begin
		return latin_1.space & level_string (A .. B);
	end to_string;

	function to_power_level (level : in string) return type_power_level is 
	-- Converts the power level (like positive) to power level (like LEVEL_POSITIVE).
	-- The prefix LEVEL_ is prepended.
	begin
		return type_power_level'value ("LEVEL_" & level);
	end to_power_level;



	function to_string (
		appearance	: in type_device_appearance;
		verbose		: in boolean := false)
		return string is
	-- Returns the given component appearance as string.
	begin
		if verbose then
			case appearance is
				when sch =>
					return ("appears in schematic only (virtual component)");
				when sch_pcb =>
					return ("appears in schematic and layout");
				when pcb =>
					return ("appears in layout only (mechanical component)");
			end case;
		else
			return latin_1.space & to_lower (type_device_appearance'image (appearance));
		end if;
	end to_string;

	function to_appearance (appearance : in string) return type_device_appearance is begin
		return type_device_appearance'value (appearance);
	end to_appearance;	





	function to_string (filled : in type_circle_filled) return string is begin
		return latin_1.space & to_lower (type_circle_filled'image (filled));
	end to_string;

	function to_circle_filled (filled : in string) return type_circle_filled is begin
		return type_circle_filled'value (filled);
	end to_circle_filled;





	function to_string (name : in type_symbol_model_file.bounded_string) 
		return string is
	begin
		return type_symbol_model_file.to_string (name);
	end to_string;

	function to_file_name (name : in string)
		return type_symbol_model_file.bounded_string is
	begin
		return type_symbol_model_file.to_bounded_string (name);
	end;






	function locate (symbol : in type_symbol_model_file.bounded_string) -- ../libraries/symbols/NAND.sym
		return type_symbols.cursor is
	begin
		return type_symbols.find (symbols, symbol);
	end locate;



	





	
end et_symbols;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
