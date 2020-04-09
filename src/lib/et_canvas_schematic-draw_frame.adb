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
with cairo;						use cairo;
with pango.layout;				use pango.layout;

with et_coordinates;			use et_coordinates;
use et_coordinates.geometry;

with et_text;
with et_schematic;				--use et_schematic;
with et_project;				use et_project;
use et_project.type_modules;

separate (et_canvas_schematic)

procedure draw_frame (
	self	: not null access type_view;
	in_area	: in type_rectangle := no_rectangle;
	context : in type_draw_context) is

	type type_line is new et_schematic.pac_shapes.type_line with null record;
	line : type_line;

	procedure draw_line is begin
		pac_draw_misc.draw_line (
			area		=> in_area,
			context		=> context,
			line		=> line,
			height		=> self.drawing.frame_bounding_box.height);
	end draw_line;

	use et_frames;
	use pac_lines;

	procedure draw_border is begin
	-- OUTER BORDER
		-- left line from bottom to top
		line.start_point := type_point (set (0.0, 0.0));
		
		line.end_point := type_point (set (
			x => 0.0,
			y => type_distance_positive (self.drawing.frame.size.y)));
		
		draw_line;

		-- right line from bottom to top
		line.start_point := type_point (set (
			x => type_distance_positive (self.drawing.frame.size.x),
			y => 0.0));
		
		line.end_point := type_point (set (
			x => type_distance_positive (self.drawing.frame.size.x),
			y => type_distance_positive (self.drawing.frame.size.y)));
		
		draw_line;

		-- lower line from left to right
		line.start_point := type_point (set (0.0, 0.0));
		line.end_point := type_point (set (
			x => type_distance_positive (self.drawing.frame.size.x),
			y => 0.0));
		
		draw_line;

		-- upper line from left to right
		line.start_point := type_point (set (
			x => 0.0,
			y => type_distance_positive (self.drawing.frame.size.y)));
		
		line.end_point := type_point (set (
			x => type_distance_positive (self.drawing.frame.size.x),
			y => type_distance_positive (self.drawing.frame.size.y)));
		
		draw_line;

		
	-- INNER BORDER
		-- left line from bottom to top
		line.start_point := type_point (set (
			x => type_distance_positive (self.drawing.frame.border_width),
			y => type_distance_positive (self.drawing.frame.border_width)));
		
		line.end_point := type_point (set (
			x => type_distance_positive (self.drawing.frame.border_width),
			y => type_distance_positive (self.drawing.frame.size.y - self.drawing.frame.border_width)));
		
		draw_line;

		-- right line from bottom to top
		line.start_point := type_point (set (
			x => type_distance_positive (self.drawing.frame.size.x - self.drawing.frame.border_width),
			y => type_distance_positive (self.drawing.frame.border_width)));
		
		line.end_point := type_point (set (
			x => type_distance_positive (self.drawing.frame.size.x - self.drawing.frame.border_width),
			y => type_distance_positive (self.drawing.frame.size.y - self.drawing.frame.border_width)));
		
		draw_line;

		-- lower line from left to right
		line.start_point := type_point (set (
			x => type_distance_positive (self.drawing.frame.border_width),
			y => type_distance_positive (self.drawing.frame.border_width)));
		
		line.end_point := type_point (set (
			x => type_distance_positive (self.drawing.frame.size.x - self.drawing.frame.border_width),
			y => type_distance_positive (self.drawing.frame.border_width)));
		
		draw_line;

		-- upper line from left to right
		line.start_point := type_point (set (
			x => type_distance_positive (self.drawing.frame.border_width),
			y => type_distance_positive (self.drawing.frame.size.y - self.drawing.frame.border_width)));
		
		line.end_point := type_point (set (
			x => type_distance_positive (self.drawing.frame.size.x - self.drawing.frame.border_width),
			y => type_distance_positive (self.drawing.frame.size.y - self.drawing.frame.border_width)));
		
		draw_line;
		
	end draw_border;

	-- Draw the line of the title block. The line is offset by the position of the title block.
	procedure query_line (cursor : in pac_lines.cursor) is begin

		line.start_point := type_point (set (
			x => type_distance_positive (element (cursor).start_point.x + self.drawing.title_block_position.x),
			y => type_distance_positive (element (cursor).start_point.y + self.drawing.title_block_position.y)));

		line.end_point := type_point (set (
			x => type_distance_positive (element (cursor).end_point.x + self.drawing.title_block_position.x),
			y => type_distance_positive (element (cursor).end_point.y + self.drawing.title_block_position.y)));

		draw_line;
	end query_line;

	procedure draw_title_block_texts is
		
		-- get placeholders common with schematic and board:
		phc : constant type_placeholders_common := 
			self.drawing.frame.title_block_schematic.placeholders;

		-- get schematic placeholders:
		phs : constant type_placeholders_schematic := 
			self.drawing.frame.title_block_schematic.additional_placeholders;

		-- get other texts
		texts : constant pac_texts.list := self.drawing.frame.title_block_schematic.texts;

		use et_text;
		
		procedure draw (
			content	: in type_text_content.bounded_string;
			size	: in type_text_size;
			font	: in type_font;
			pos		: in et_frames.type_position) is

			-- The given position is given in frame coordinates and must be 
			-- converted to schematic coordinates and shifted by the position
			-- of the title block.
			ps : constant geometry.type_point := type_point (set (
					x => type_distance_positive (pos.x + self.drawing.title_block_position.x),
					y => type_distance_positive (pos.y + self.drawing.title_block_position.y)));
		begin
			pac_draw_misc.draw_text (
				area		=> in_area,
				context		=> context,
				content		=> content,
				size		=> type_distance_positive (size),
				font		=> font,
				position	=> ps,
				origin		=> true,
				rotation	=> zero_rotation,
				alignment	=> (LEFT, BOTTOM),
				height		=> self.drawing.frame_bounding_box.height);
		end draw;

	begin -- draw_title_block_texts
	-- COMMON PLACEHOLDERS
		-- project name:
		draw (
			content	=> to_content ("Project Name"), -- CS
			size	=> phc.project_name.size,
			font	=> font_placeholders,
			pos		=> phc.project_name.position);
		
		-- module file name:
		draw (
			content	=> to_content ("Module File Name"), -- CS
			size	=> phc.module_file_name.size,
			font	=> font_placeholders,
			pos		=> phc.module_file_name.position);

		-- active assembly variant:
		draw (
			content	=> to_content ("Assembly Veriant"), -- CS
			size	=> phc.active_assembly_variant.size,
			font	=> font_placeholders,
			pos		=> phc.active_assembly_variant.position);

	-- PLACEHOLDERS
		-- company
		draw (
			content	=> to_content ("company"), -- CS
			size	=> phs.company.size,
			font	=> font_placeholders,
			pos		=> phs.company.position);

		-- company
		draw (
			content	=> to_content ("customer"), -- CS
			size	=> phs.customer.size,
			font	=> font_placeholders,
			pos		=> phs.customer.position);

		-- partcode
		draw (
			content	=> to_content ("partcode"), -- CS
			size	=> phs.partcode.size,
			font	=> font_placeholders,
			pos		=> phs.partcode.position);

		-- drawing number
		draw (
			content	=> to_content ("drawing number"), -- CS
			size	=> phs.drawing_number.size,
			font	=> font_placeholders,
			pos		=> phs.drawing_number.position);

		-- revision
		draw (
			content	=> to_content ("revision"), -- CS
			size	=> phs.revision.size,
			font	=> font_placeholders,
			pos		=> phs.revision.position);

		-- drawn by
		draw (
			content	=> to_content ("drawn by"), -- CS
			size	=> phs.drawn_by.size,
			font	=> font_placeholders,
			pos		=> phs.drawn_by.position);

		-- checked by
		draw (
			content	=> to_content ("checked by"), -- CS
			size	=> phs.checked_by.size,
			font	=> font_placeholders,
			pos		=> phs.checked_by.position);

		-- approved by
		draw (
			content	=> to_content ("approved by"), -- CS
			size	=> phs.approved_by.size,
			font	=> font_placeholders,
			pos		=> phs.approved_by.position);

		-- drawn date
		draw (
			content	=> to_content ("drawn date"), -- CS
			size	=> phs.drawn_date.size,
			font	=> font_placeholders,
			pos		=> phs.drawn_date.position);

		-- checked date
		draw (
			content	=> to_content ("checked date"), -- CS
			size	=> phs.checked_date.size,
			font	=> font_placeholders,
			pos		=> phs.checked_date.position);

		-- approved date
		draw (
			content	=> to_content ("approved date"), -- CS
			size	=> phs.approved_date.size,
			font	=> font_placeholders,
			pos		=> phs.approved_date.position);
		
		
	-- ADDITIONAL PLACEHOLDERS
		
		-- sheet number n of m
		draw (
			content	=> to_content ("sheet n/m"), -- CS
			size	=> phs.sheet_number.size,
			font	=> font_placeholders,
			pos		=> phs.sheet_number.position);

		-- description
		draw (
			content	=> to_content ("description"), -- CS
			size	=> phs.description.size,
			font	=> font_placeholders,
			pos		=> phs.description.position);

		-- category
		draw (
			content	=> to_content ("cat"), -- CS
			size	=> phs.category.size,
			font	=> font_placeholders,
			pos		=> phs.category.position);
		
	-- OTHER TEXTS
		-- CS

		
	end draw_title_block_texts;
	
begin -- draw_frame
-- 	put_line ("draw frame ...");

	if (in_area = no_rectangle)
		or else intersects (in_area, self.drawing.frame_bounding_box) 
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

		-- placeholders and other texts
		draw_title_block_texts;
		
		-- CS draw the sector delimiters

		-- CS draw the sector rows and columns

		
		cairo.stroke (context.cr);
	end if;
	
end draw_frame;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
