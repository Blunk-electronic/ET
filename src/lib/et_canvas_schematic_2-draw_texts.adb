------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              DRAW TEXTS                                  --
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
--                                                                          --
------------------------------------------------------------------------------

--   For correct displaying set tab width in your editor to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--


with et_schematic_text;					use et_schematic_text;


separate (et_canvas_schematic_2)

procedure draw_texts is

	use pac_texts;
	
	use et_colors;
	use et_colors.schematic;


	
	procedure query_text (cursor : in pac_texts.cursor) is 
		use pac_draw_text;
	begin		
		-- We want to draw only those texts which are on the active sheet:
		if element (cursor).sheet = active_sheet then

			draw_text (
				content		=> element (cursor).content,
				size		=> element (cursor).size,
				font		=> text_font,
				anchor		=> type_vector_model (element (cursor).position),
				origin		=> true,

				-- This is documentational text. It is readable from the front or the right.
				rotation	=> pac_text.to_rotation (element (cursor).rotation),
				
				alignment	=> element (cursor).alignment);

		end if;
	end query_text;
		
begin
	--put_line ("draw texts ...");
	
	set_color_texts;

	iterate (element (active_module).texts, query_text'access);
	
end draw_texts;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
