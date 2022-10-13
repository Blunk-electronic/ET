------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         SCHEMATIC DRAW FRAME                             --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
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
with et_meta;

separate (et_canvas_schematic)

procedure draw_frame (
	self	: not null access type_view;
	in_area	: in type_bounding_box := no_area;
	context : in type_draw_context) 
is
	use et_frames;
	
	frame_size   : et_frames.type_frame_size renames self.get_frame.size;
	frame_height : et_frames.type_distance renames self.get_frame.size.y;
	title_block_position : et_frames.type_position renames self.get_frame.title_block_schematic.position;

	
	
	procedure draw_additional_placeholders is
		
		-- get placeholders:
		phs : constant type_placeholders_schematic := 
			self.get_frame.title_block_schematic.additional_placeholders;

		use et_text;

		
		procedure draw_sheet_description is
			use et_project;

			-- Get the description of the current active sheet:
			des : constant type_schematic_description := 
					sheet_description (current_active_module, current_active_sheet);
		begin
			-- category (development, product, routing)
			draw_text (
				area	=> in_area,
				context	=> context,		  
				content	=> to_content (to_string (des.category)),
				size	=> phs.category.size,
				font	=> font_placeholders,
				pos		=> phs.category.position,
				tb_pos	=> title_block_position,
				height	=> frame_height
				);

			-- description
			draw_text (
				area	=> in_area,
				context	=> context,		  
				content	=> to_content (to_string (des.content)),
				size	=> phs.description.size,
				font	=> font_placeholders,
				pos		=> phs.description.position,
				tb_pos	=> title_block_position,
				height	=> frame_height
				);
						
		end draw_sheet_description;

		
	begin -- draw_additional_placeholders
		
		-- sheet number n of m
		draw_text (
			area	=> in_area,
			context	=> context,		  
			content	=> to_content (to_sheet (current_active_sheet)), -- CS complete with "/of total"
			size	=> phs.sheet_number.size,
			font	=> font_placeholders,
			pos		=> phs.sheet_number.position,
			tb_pos	=> title_block_position,
			height	=> frame_height);


		draw_sheet_description;
		
	end draw_additional_placeholders;

	
begin -- draw_frame
-- 	put_line ("draw frame ...");

	-- We draw the frame if it is inside the given area or if it itersects the given area:
	if (in_area = no_area)
		or else intersects (in_area, self.frame_bounding_box) 
	then
		-- CS test size 
-- 			if not size_above_threshold (self, context.view) then
-- 				return;
-- 			end if;

		cairo.set_line_width (context.cr, line_width_thin);

		set_color_frame (context.cr);

		-- FRAME BORDER
		draw_border (
			area			=> in_area,
			context			=> context,
			frame_size		=> frame_size,
			border_width	=> self.get_frame.border_width,
			height			=> frame_height);

		
		-- TITLE BLOCK
		-- lines
		--iterate (self.get_frame.title_block_schematic.lines, query_line'access);
		--cairo.stroke (context.cr);

		draw_title_block_lines (
			area		=> in_area,
			context		=> context,
			lines		=> self.get_frame.title_block_schematic.lines,
			tb_pos		=> title_block_position,
			frame_size	=> frame_size);

		
		-- draw common placeholders and other texts
		draw_texts (
			area		=> in_area,
			context		=> context,
			ph_common	=> self.get_frame.title_block_schematic.placeholders,
			ph_basic	=> type_placeholders_basic (self.get_frame.title_block_schematic.additional_placeholders),
			texts		=> self.get_frame.title_block_schematic.texts,
			meta		=> et_meta.type_basic (element (current_active_module).meta.board),
			tb_pos		=> title_block_position,
			height		=> frame_height);


		draw_additional_placeholders;
		
		-- draw the sector delimiters
		draw_sector_delimiters (
			area			=> in_area,
			context			=> context,
			sectors			=> self.get_frame.sectors,
			frame_size		=> frame_size,
			border_width	=> self.get_frame.border_width);

	end if;
	
end draw_frame;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
