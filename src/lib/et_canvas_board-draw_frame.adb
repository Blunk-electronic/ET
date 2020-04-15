------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           BOARD DRAW FRAME                               --
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

separate (et_canvas_board)

procedure draw_frame (
	self	: not null access type_view;
	in_area	: in type_rectangle := no_rectangle;
	context : in type_draw_context) is

	use et_frames;
	use et_project.type_modules;
	use et_canvas_schematic;
	
	package pac_draw_frame is new et_canvas_draw_frame.pac_draw_frame (
		draw_ops		=> et_canvas_board.pac_draw_package,
		in_area			=> in_area,
		context			=> context,
		frame_height	=> self.drawing.frame_bounding_box.height,
		frame_size		=> self.drawing.frame.frame.size,
		border_width	=> self.drawing.frame.frame.border_width,
		sectors			=> self.drawing.frame.frame.sectors,
		title_block		=> type_title_block (self.drawing.frame.frame.title_block_pcb), -- incl. common placeholders
		meta			=> et_meta.type_basic (element (current_active_module).meta.board),
		placeholders	=> type_placeholders_basic (self.drawing.frame.frame.title_block_pcb.additional_placeholders)
		);

	use pac_draw_frame;

	procedure draw_additional_placeholders is 

		-- get placeholders:
		phs : constant type_placeholders_pcb := 
			self.drawing.frame.frame.title_block_pcb.additional_placeholders;

		use et_text;
	begin
		draw_text (
			content	=> to_content ("1..16"), -- CS
			size	=> phs.signal_layer.size,
			font	=> font_placeholders,
			pos		=> phs.signal_layer.position);

		draw_text (
			content	=> to_content ("TOP/BOT"), -- CS
			size	=> phs.face.size,
			font	=> font_placeholders,
			pos		=> phs.face.position);

	end draw_additional_placeholders;

	procedure draw_cam_markers is

		cms : constant type_cam_markers :=
			self.drawing.frame.frame.title_block_pcb.cam_markers;

		use et_text;
	begin
		draw_text (
			content	=> to_content (to_string (cms.face.content)),
			size	=> cms.face.size,
			font	=> font_placeholders,
			pos		=> cms.face.position);

		draw_text (
			content	=> to_content (to_string (cms.silk_screen.content)),
			size	=> cms.silk_screen.size,
			font	=> font_placeholders,
			pos		=> cms.silk_screen.position);

		draw_text (
			content	=> to_content (to_string (cms.assy_doc.content)),
			size	=> cms.assy_doc.size,
			font	=> font_placeholders,
			pos		=> cms.assy_doc.position);

		draw_text (
			content	=> to_content (to_string (cms.keepout.content)),
			size	=> cms.keepout.size,
			font	=> font_placeholders,
			pos		=> cms.keepout.position);

		draw_text (
			content	=> to_content (to_string (cms.plated_millings.content)),
			size	=> cms.plated_millings.size,
			font	=> font_placeholders,
			pos		=> cms.plated_millings.position);

		draw_text (
			content	=> to_content (to_string (cms.pcb_outline.content)),
			size	=> cms.pcb_outline.size,
			font	=> font_placeholders,
			pos		=> cms.pcb_outline.position);

		draw_text (
			content	=> to_content (to_string (cms.route_restrict.content)),
			size	=> cms.route_restrict.size,
			font	=> font_placeholders,
			pos		=> cms.route_restrict.position);

		draw_text (
			content	=> to_content (to_string (cms.via_restrict.content)),
			size	=> cms.via_restrict.size,
			font	=> font_placeholders,
			pos		=> cms.via_restrict.position);

		draw_text (
			content	=> to_content (to_string (cms.signal_layer.content)),
			size	=> cms.signal_layer.size,
			font	=> font_placeholders,
			pos		=> cms.signal_layer.position);
		
	end draw_cam_markers;
	
begin -- draw_frame
--		put_line ("draw frame ...");

	if (in_area = no_rectangle)
		or else intersects (in_area, self.drawing.frame_bounding_box) 
	then
		-- CS test size 
-- 			if not size_above_threshold (self, context.view) then
-- 				return;
-- 			end if;

		
		cairo.set_line_width (context.cr, line_width_thin);

		cairo.set_source_rgb (context.cr, gdouble (1), gdouble (0), gdouble (0)); -- red

		
		-- frame border
		draw_border;

		-- title block lines
		pac_lines.iterate (self.drawing.frame.frame.title_block_pcb.lines, query_line'access);
		cairo.stroke (context.cr);
		
		-- draw the sector delimiters
		draw_sector_delimiters;

		-- draw common placeholders and other texts
		draw_texts;

		draw_additional_placeholders;
		
		draw_cam_markers;
		
	end if;
	
end draw_frame;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
