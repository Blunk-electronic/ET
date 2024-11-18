------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              ALIGNMENT                                   --
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

with ada.text_io;					use ada.text_io;
with ada.characters.handling;		use ada.characters.handling;
with et_logging;
with et_keywords;					use et_keywords;


package body et_alignment is

	function to_string (
		alignment : in type_text_alignment_horizontal) 
		return string 
	is 
		s : string := to_lower (type_text_alignment_horizontal'image (alignment));
	begin
		return s (alignment_prefix'length + 1 .. s'last); 
	end;

	
	function to_alignment_horizontal (
		alignment : in string) 
		return type_text_alignment_horizontal 
	is begin
		return type_text_alignment_horizontal'value (alignment_prefix & alignment);
	end;

	
	function to_string (
		alignment : in type_text_alignment_vertical) 
		return string 
	is 
		s : string := to_lower (type_text_alignment_vertical'image (alignment));
	begin
		return s (alignment_prefix'length + 1 .. s'last); 
	end;

	
	function to_alignment_vertical (
		alignment : in string) 
		return type_text_alignment_vertical 
	is begin
		return type_text_alignment_vertical'value (alignment_prefix & alignment);
	end;

	
	function to_alignment (
		line : in type_fields_of_line; -- "alignment horizontal center vertical center"
		from : in type_field_count_positive)
		return type_text_alignment 
	is
		use et_logging;
		
		function f (
			line		: in type_fields_of_line;
			position	: in type_field_count_positive) 
			return string renames get_field;
		
		
		alignment : type_text_alignment; -- to be returned

		place : type_field_count_positive := from; -- the field being read from given line

		-- CS: flags to detect missing sheet, x or y
	begin
		while place <= field_count (line) loop

			-- We expect after the "horizontal" the horizontal alignment
			if f (line, place) = keyword_horizontal then
				alignment.horizontal := to_alignment_horizontal (f (line, place + 1));

			-- We expect after the "vertical" the vertical alignment
			elsif f (line, place) = keyword_vertical then
				alignment.vertical := to_alignment_vertical (f (line, place + 1));
				
			else
				log (ERROR, "alignment invalid: " & (f (line, place)));
				raise constraint_error;
			end if;
				
			place := place + 2;
		end loop;
		
		return alignment;
	end to_alignment;

	
	function to_string (alignment : in type_text_alignment) return string is
	begin
		return "alignment (hor./vert.) "
			& to_string (alignment.horizontal)
			& " / "
			& to_string (alignment.vertical);
	end to_string;



end et_alignment;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
