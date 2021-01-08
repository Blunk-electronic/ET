------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           BOARD DRAW FRAME                               --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2021 Mario Blunk, Blunk electronic          --
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
with ada.characters.handling;	use ada.characters.handling;
with et_text;
with et_canvas_draw_frame;
with et_meta;

separate (et_canvas_board)

procedure draw_frame (
	self	: not null access type_view;
	in_area	: in type_rectangle := no_rectangle;
	context : in type_draw_context) 
is
	use et_frames;
	use et_canvas_schematic;
	
	package pac_draw_frame is new et_canvas_draw_frame.pac_draw_frame (
		draw_ops		=> pac_draw_fab,
		in_area			=> in_area,
		context			=> context,
		frame_size		=> self.get_frame.size,
		border_width	=> self.get_frame.border_width,
		sectors			=> self.get_frame.sectors,
		title_block		=> type_title_block (self.get_frame.title_block_pcb), -- incl. common placeholders
		meta			=> et_meta.type_basic (element (current_active_module).meta.board),
		placeholders	=> type_placeholders_basic (self.get_frame.title_block_pcb.additional_placeholders)
		);

	use pac_draw_frame;

	procedure draw_cam_markers is
		cms : constant type_cam_markers := self.get_frame.title_block_pcb.cam_markers;

		use et_text;
		use et_display;
		use et_display.board;

		-- These flags indicate whether a top or bottom layer is shown.
		-- If none of them is true, no cam marker for face and no face itself
		-- will be displayed at all.
		top_enabled, bottom_enabled : boolean := false;

		procedure evaluate_face is begin

			-- Draw the CAM marker "FACE:" or "SIDE:" if a top or bottom layers is enabled.
			if top_enabled or bottom_enabled then
				draw_text (
					content	=> to_content (to_string (cms.face.content)), -- "FACE:" or "SIDE:"
					size	=> cms.face.size,
					font	=> font_placeholders,
					pos		=> cms.face.position);

				-- Draw the face like "TOP" or "BOTTOM":
				if top_enabled and bottom_enabled then
					draw_text (
						content	=> to_content (type_face'image (TOP) & " & " & type_face'image (BOTTOM)), -- "TOP & BOTTOM"
						size	=> self.get_frame.title_block_pcb.additional_placeholders.face.size,
						font	=> font_placeholders,
						pos		=> self.get_frame.title_block_pcb.additional_placeholders.face.position);
					
				elsif top_enabled then
						draw_text (
							content	=> to_content (type_face'image (TOP)), -- "TOP"
							size	=> self.get_frame.title_block_pcb.additional_placeholders.face.size,
							font	=> font_placeholders,
							pos		=> self.get_frame.title_block_pcb.additional_placeholders.face.position);

				elsif bottom_enabled then
						draw_text (
							content	=> to_content (type_face'image (BOTTOM)), -- "BOTTOM"
							size	=> self.get_frame.title_block_pcb.additional_placeholders.face.size,
							font	=> font_placeholders,
							pos		=> self.get_frame.title_block_pcb.additional_placeholders.face.position);
				end if;
			end if;
		end evaluate_face;
		
		procedure silkscreen is begin
			draw_text (
				content	=> to_content (to_string (cms.silk_screen.content)),
				size	=> cms.silk_screen.size,
				font	=> font_placeholders,
				pos		=> cms.silk_screen.position);
		end silkscreen;

		procedure assy_doc is begin
			draw_text (
				content	=> to_content (to_string (cms.assy_doc.content)),
				size	=> cms.assy_doc.size,
				font	=> font_placeholders,
				pos		=> cms.assy_doc.position);
		end assy_doc;

		procedure keepout is begin
			draw_text (
				content	=> to_content (to_string (cms.keepout.content)),
				size	=> cms.keepout.size,
				font	=> font_placeholders,
				pos		=> cms.keepout.position);
		end keepout;

		procedure stop_mask is begin
			draw_text (
				content	=> to_content (to_string (cms.stop_mask.content)),
				size	=> cms.stop_mask.size,
				font	=> font_placeholders,
				pos		=> cms.stop_mask.position);
		end stop_mask;

		procedure stencil is begin
			draw_text (
				content	=> to_content (to_string (cms.stencil.content)),
				size	=> cms.stencil.size,
				font	=> font_placeholders,
				pos		=> cms.stencil.position);
		end stencil;
		
	begin -- draw_cam_markers

		-- silkscreen
		if silkscreen_enabled (TOP) then
			silkscreen;
			top_enabled := true;
		end if;

		if silkscreen_enabled (BOTTOM) then
			silkscreen;
			bottom_enabled := true;
		end if;

		-- assy doc
		if assy_doc_enabled (TOP) then
			assy_doc;
			top_enabled := true;
		end if;
		
		if assy_doc_enabled (BOTTOM) then
			assy_doc;
			bottom_enabled := true;
		end if;

		-- keepout
		if keepout_enabled (TOP) then
			keepout;
			top_enabled := true;
		end if;

		if keepout_enabled (BOTTOM) then
			keepout;
			bottom_enabled := true;
		end if;

		-- plated millings
		if plated_millings_enabled then
			draw_text (
				content	=> to_content (to_string (cms.plated_millings.content)),
				size	=> cms.plated_millings.size,
				font	=> font_placeholders,
				pos		=> cms.plated_millings.position);
		end if;

		-- outline
		if outline_enabled then
			draw_text (
				content	=> to_content (to_string (cms.pcb_outline.content)),
				size	=> cms.pcb_outline.size,
				font	=> font_placeholders,
				pos		=> cms.pcb_outline.position);
		end if;

		-- stop mask
		if stop_mask_enabled (TOP) then
			stop_mask;
			top_enabled := true;
		end if;
	
		if stop_mask_enabled (BOTTOM) then
			stop_mask;
			bottom_enabled := true;
		end if;

		-- stencil
		if stencil_enabled (TOP) then
			stencil;
			top_enabled := true;
		end if;

		if stencil_enabled (BOTTOM) then
			stencil;
			bottom_enabled := true;
		end if;
		
		-- route restrict
		if route_restrict_enabled then
			draw_text (
				content	=> to_content (to_string (cms.route_restrict.content)),
				size	=> cms.route_restrict.size,
				font	=> font_placeholders,
				pos		=> cms.route_restrict.position);
		end if;

		-- via restrict
		if via_restrict_enabled then
			draw_text (
				content	=> to_content (to_string (cms.via_restrict.content)),
				size	=> cms.via_restrict.size,
				font	=> font_placeholders,
				pos		=> cms.via_restrict.position);
		end if;

		-- conductor layers
		if conductors_enabled then
			draw_text (
				content	=> to_content (to_string (cms.signal_layer.content)), -- "SGNL_LYR:"
				size	=> cms.signal_layer.size,
				font	=> font_placeholders,
				pos		=> cms.signal_layer.position);

			draw_text (
				content	=> to_content (enabled_conductor_layers), -- "1,2,5..32"
				size	=> self.get_frame.title_block_pcb.additional_placeholders.signal_layer.size,
				font	=> font_placeholders,
				pos		=> self.get_frame.title_block_pcb.additional_placeholders.signal_layer.position);
		end if;

		evaluate_face;
		
	end draw_cam_markers;
	
begin -- draw_frame
--		put_line ("draw frame ...");

	if (in_area = no_rectangle)
		or else intersects (in_area, self.frame_bounding_box) 
	then
		-- CS test size 
-- 			if not size_above_threshold (self, context.view) then
-- 				return;
-- 			end if;

		
		cairo.set_line_width (context.cr, line_width_thin);

		set_color_frame (context.cr);

		
		-- frame border
		draw_border;

		-- title block lines
		pac_lines.iterate (self.get_frame.title_block_pcb.lines, query_line'access);
		
		-- draw the sector delimiters
		draw_sector_delimiters;

		-- draw common placeholders and other texts
		draw_texts;
		
		draw_cam_markers;
		
	end if;
	
end draw_frame;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
