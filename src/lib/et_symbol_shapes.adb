------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                      SYMBOL PRIMITIVE SHAPES                             --
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

-- with ada.exceptions; 			use ada.exceptions;



package body et_symbol_shapes is


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

	
	
end et_symbol_shapes;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
