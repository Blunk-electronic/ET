------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         SCHEMATIC DRAW FRAME                             --
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

with ada.text_io;				use ada.text_io;

with et_text;
with et_canvas_draw_frame;
with et_meta;
with et_project;

separate (et_canvas_schematic)

procedure draw_frame (
	self	: not null access type_view;
	in_area	: in type_rectangle := no_rectangle;
	context : in type_draw_context) is

	use et_frames;
	use et_project.type_modules;
	
	package pac_draw_frame is new et_canvas_draw_frame.pac_draw_frame (
		draw_ops		=> et_canvas_schematic.pac_draw_misc,
		in_area			=> in_area,
		context			=> context,
		frame_size		=> self.drawing.frame.size,
		border_width	=> self.drawing.frame.border_width,
		sectors			=> self.drawing.frame.sectors,
		title_block		=> type_title_block (self.drawing.frame.title_block_schematic), -- incl. common placeholders
		meta			=> et_meta.type_basic (element (current_active_module).meta.schematic),
		placeholders	=> type_placeholders_basic (self.drawing.frame.title_block_schematic.additional_placeholders)
		);

	use pac_draw_frame;
	use pac_lines;

	procedure draw_additional_placeholders is
		
		-- get placeholders:
		phs : constant type_placeholders_schematic := 
			self.drawing.frame.title_block_schematic.additional_placeholders;

		use et_text;

		procedure draw_sheet_description is
			use et_project;

			-- Get the description of the current active sheet:
			des : constant type_schematic_description := 
					sheet_description (current_active_module, self.drawing.sheet);
		begin
			-- category (development, product, routing)
			draw_text (
				content	=> to_content (to_string (des.category)),
				size	=> phs.category.size,
				font	=> font_placeholders,
				pos		=> phs.category.position);

			-- description
			draw_text (
				content	=> to_content (to_string (des.content)),
				size	=> phs.description.size,
				font	=> font_placeholders,
				pos		=> phs.description.position);
			
		end draw_sheet_description;
		
	begin -- draw_additional_placeholders
		
		-- sheet number n of m
		draw_text (
			content	=> to_content (to_sheet (self.drawing.sheet)), -- CS complete with "/of total"
			size	=> phs.sheet_number.size,
			font	=> font_placeholders,
			pos		=> phs.sheet_number.position);

		draw_sheet_description;
		
	end draw_additional_placeholders;
	
begin -- draw_frame
-- 	put_line ("draw frame ...");

	-- We draw the frame if it is inside the given area or if it itersects the given area:
	if (in_area = no_rectangle)
		or else intersects (in_area, self.frame_bounding_box) 
	then
		-- CS test size 
-- 			if not size_above_threshold (self, context.view) then
-- 				return;
-- 			end if;

		cairo.set_line_width (context.cr, line_width_thin);

		cairo.set_source_rgb (context.cr, gdouble (1), gdouble (0), gdouble (0)); -- red

		-- FRAME BORDER
		draw_border;
		
		-- TITLE BLOCK
		-- lines
		iterate (self.drawing.frame.title_block_schematic.lines, query_line'access);
		cairo.stroke (context.cr);
		
		-- draw common placeholders and other texts
		draw_texts;

		draw_additional_placeholders;
		
		-- draw the sector delimiters
		draw_sector_delimiters;

	end if;
	
end draw_frame;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
