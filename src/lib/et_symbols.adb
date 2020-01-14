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
	end;
	
	function to_text_style (style : in string) return type_text_style is begin
		return type_text_style'value (style);
	end;



	function to_string (meaning : in type_placeholder_meaning) return string is begin
		return to_lower (type_placeholder_meaning'image (meaning));
	end;

	function to_meaning (meaning : in string) return type_placeholder_meaning is begin
		return type_placeholder_meaning'value (meaning);
	end;





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

	

	function to_string (port : in type_port_name.bounded_string) return string is begin
		return type_port_name.to_string (port);
	end;

	function to_port_name (name : in string) return type_port_name.bounded_string is begin
		return type_port_name.to_bounded_string (name);
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

	procedure compute_boundaries ( 
		symbol			: in type_symbols.cursor;
		log_threshold	: in et_string_processing.type_log_level) is
		use et_string_processing;

		-- Initially the symbol.boundaries are not determined. The operator
		-- is not responsible for setting them.
		-- All elements of the symbol must be probed and the greatest and
		-- smallest x and y positions detected.
		-- Thus the boundaries of the symbol are updated many times here.
		
		-- CS consider mirror status of symbol !

		procedure query_items (
			symbol_name	: in type_symbol_model_file.bounded_string; -- ../libraries/symbols/NAND.sym
			symbol		: in out type_symbol) is 

			use type_lines;
			use type_circles;
			use type_arcs;
			use type_ports;
			use type_texts;

			procedure query_line (c : in type_lines.cursor) is begin
				union (symbol.boundaries, boundaries (element (c)));
			end;

			procedure query_circle (c : in type_circles.cursor) is begin
				union (symbol.boundaries, boundaries (element (c)));
			end;

			procedure query_arc (c : in type_arcs.cursor) is begin
				union (symbol.boundaries, boundaries (element (c)));
			end;

			procedure query_port (c : in type_ports.cursor) is begin
				-- The port position is the point of connection with a net.
				-- Regardless of the rotation or length of the port,
				-- this end of the port points always away from the
				-- symbol center.
				union (symbol.boundaries, element (c).position);
			end;

			procedure query_text (c : in type_texts.cursor) is begin
				-- CS Currently we care for the position of the text
				-- only. The text length and size is ignored.
				union (symbol.boundaries, element (c).position);
			end;
			
		begin -- query_items

			-- Probe elements of the symbol and unite them in symbol.boundaries:
			iterate (symbol.shapes.lines, query_line'access);
			iterate (symbol.shapes.circles, query_circle'access);
			iterate (symbol.shapes.arcs, query_arc'access);
			iterate (symbol.ports, query_port'access);
			iterate (symbol.texts, query_text'access);
			
			-- Probe placeholders in case the symbol belongs to
			-- a real device:
			if symbol.appearance = PCB then
				union (symbol.boundaries, symbol.name.position);
				union (symbol.boundaries, symbol.value.position);
				union (symbol.boundaries, symbol.purpose.position);
			end if;
			
		end query_items;
		
	begin -- compute_boundaries
		log (text => "computing boundaries ...", level => log_threshold);

		type_symbols.update_element (
			container	=> symbols,
			position	=> symbol,
			process		=> query_items'access);
		
	end compute_boundaries;
	
end et_symbols;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
