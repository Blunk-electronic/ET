------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                         SCHEMATIC SYMBOLS                                --
--                                                                          --
--                               B o d y                                    --
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
with ada.characters.handling;	use ada.characters.handling;

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




	function get_A (
		line : in pac_symbol_lines.cursor)
		return type_vector_model
	is begin
		return get_A (element (line));
	end;


	function get_B (
		line : in pac_symbol_lines.cursor)
		return type_vector_model
	is begin
		return get_B (element (line));
	end;


	


	procedure set_width (
		arc		: in out type_symbol_arc;
		width	: in type_line_width)
	is begin
		arc.width := width;
	end set_width;

	

	function get_A (
		arc : in pac_symbol_arcs.cursor)
		return type_vector_model
	is begin
		return get_A (element (arc));
	end;


	function get_B (
		arc : in pac_symbol_arcs.cursor)
		return type_vector_model
	is begin
		return get_B (element (arc));
	end;


	function get_center (
		arc : in pac_symbol_arcs.cursor)
		return type_vector_model
	is begin
		return get_center (element (arc));
	end;


	function get_direction (
		arc : in pac_symbol_arcs.cursor)
		return type_direction_of_rotation
	is begin
		return get_direction (element (arc));
	end;
	


	function is_real (
		symbol : in type_symbol)
		return boolean
	is begin
		case symbol.appearance is
			when APPEARANCE_PCB		=> return true;
			when APPEARANCE_VIRTUAL	=> return false;
		end case;
	end;



	

	function get_port_positions (
		symbol	: in type_symbol)
		return pac_points.list
	is
		use pac_points;
		result : pac_points.list;

		procedure query_port (c : in pac_ports.cursor) is begin
			result.append (get_position (c));
		end query_port;
		
	begin
		symbol.ports.iterate (query_port'access);
		return result;
	end get_port_positions;



	

	procedure reset_arc (
		arc	: in out type_symbol_arc)
	is 
		a : type_arc;
	begin
		arc := (a with type_line_width'first);
	end reset_arc;

	
	



	function to_string (filled : in type_circle_filled) return string is begin
		return to_lower (type_circle_filled'image (filled));
	end;

	function to_circle_filled (filled : in string) return type_circle_filled is begin
		return type_circle_filled'value (filled);
	end;



	procedure set_width (
		circle	: in out type_symbol_circle;
		width	: in type_line_width)
	is begin
		circle.width := width;
	end set_width;

	


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






	procedure locate_symbol (
		model_file	: in pac_symbol_model_file.bounded_string;
		cursor		: in out pac_symbols.cursor)
	is begin
		cursor := symbol_library.find (model_file);
	end locate_symbol;

	

	
	
	function is_real (
		symbol : in pac_symbols.cursor)
		return boolean
	is begin
		case element (symbol).appearance is
			when APPEARANCE_PCB		=> return true;
			when APPEARANCE_VIRTUAL	=> return false;
		end case;
	end is_real;



	function get_port_positions (
		symbol	: in pac_symbols.cursor)
		return pac_points.list
	is begin
		return get_port_positions (element (symbol));
	end get_port_positions;

	
	
	
end et_symbols;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
