------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              DRAW TEXTS                                  --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
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

with et_project;				use et_project;

with et_schematic;				use et_schematic;
use et_schematic.pac_texts;
use et_project.type_modules;

separate (et_canvas_schematic)

procedure draw_texts (
	self	: not null access type_view;
	in_area	: in type_rectangle := no_rectangle;
	context : in type_draw_context) is

	procedure query_text (cursor : in pac_texts.cursor) is begin
		
		-- We want to draw only those texts which are on the active sheet:
		if element (cursor).sheet = current_active_sheet then

			pac_draw_misc.draw_text (
				area		=> in_area,
				context		=> context,
				content		=> element (cursor).content,
				size		=> element (cursor).size,
				font		=> text_font,
				position	=> type_point (element (cursor).position),
				origin		=> true,

				-- This is documentational text. It is readable from the front or the right.
				rotation	=> pac_text.to_rotation (element (cursor).rotation),
				
				alignment	=> element (cursor).alignment,
				height		=> self.frame_height);

		end if;
	end query_text;
		
begin
	--put_line ("draw texts ...");
	
	set_color_texts (context.cr);

	iterate (element (current_active_module).texts, query_text'access);
	
end draw_texts;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
