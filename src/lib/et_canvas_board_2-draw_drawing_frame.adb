------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           BOARD DRAW FRAME                               --
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


separate (et_canvas_board_2)

procedure draw_drawing_frame is

	use et_colors.board;
	use et_frames;
	use pac_drawing_frame;
	

	-- Get the frame of the board.
	-- NOTE: In the board domain we have only one sheet
	--       and hence only one frame.
	frame : type_frame_pcb renames element (active_module).board.frame;
	-- NOTE: The rename here serves just as a shortcut to the frame.

	
	-- Get the title block:
	title_block : type_title_block_pcb renames frame.frame.title_block_pcb;
	-- NOTE: The rename here serves just as a shortcut to the title block.


	-- The position of the title block.
	-- The block has no rotation. The x/y position will be
	-- extracted later:
	tb_position : pac_geometry.type_position := 
		(rotation => 0.0, others => <>);


	

	-- This procedure draws the lines of the title block:
	procedure draw_title_block_lines is

		use pac_lines;
	
		procedure query_line (c : in pac_lines.cursor) is
			l1 : et_frames.type_line renames element (c);
			l2 : pac_geometry.type_line;
		begin
			l2 := to_line (l1);

			-- The width of 0.0 has no meaning because 
			-- the argument do_stroke is false by default
			-- (see specs of draw_line):
			draw_line (l2, tb_position, 0.0);
		end query_line;

		
	begin
		set_linewidth (linewidth_1);
		
		iterate (
			container	=> title_block.lines, 
			process		=> query_line'access);
			
		stroke;
	end draw_title_block_lines;




	
		
	procedure draw_cam_markers is

		-- Get the CAM markers of the title block.
		-- The 'rename' statement serves just as a shortcut.
		cms : type_cam_markers renames title_block.cam_markers;

		-- Get additional placeholders of the title block.
		-- The 'rename' statement serves just as a shortcut.
		aps : type_placeholders_pcb renames title_block.placeholders_additional;
		

		use et_text;
		use pac_draw_text;

		-- These flags indicate whether a top or bottom layer is shown.
		-- If none of them is true, no cam marker for face and no face itself
		-- will be displayed at all.
		top_enabled, bottom_enabled : boolean := false;


		-- A temporarily storage place for the
		-- position of a placeholder or a CAM marker:
		pos : type_vector_model;		


		-- This procedure drawns something like "SIDE: TOP"
		procedure draw_side_name is 
			
			procedure top_bottom (
				face : in string)
			is begin
				pos := to_vector (aps.face.position);
				
				draw_text (
					content		=> to_content (face),
					size		=> to_distance (aps.face.size),
					font		=> font_placeholders,
					anchor		=> add (pos, tb_position.place),
					origin		=> false,
					rotation	=> 0.0,
					alignment	=> (ALIGN_LEFT, ALIGN_BOTTOM));
			end top_bottom;

			
		begin			
			-- Draw the CAM marker "SIDE:" 
			-- if any top or bottom layer is enabled:
			if top_enabled or bottom_enabled then
				pos := to_vector (cms.face.position);
				
				draw_text (
					content		=> to_content (to_string (cms.face.content)), -- "SIDE:"
					size		=> to_distance (cms.face.size),
					font		=> font_placeholders,
					anchor		=> add (pos, tb_position.place),
					origin		=> false,
					rotation	=> 0.0,
					alignment	=> (ALIGN_LEFT, ALIGN_BOTTOM));


				-- Draw the name of the side like "TOP" or "BOTTOM":
				if top_enabled and bottom_enabled then
					top_bottom (to_string (TOP) & " & " & to_string (BOTTOM));

				elsif top_enabled then
					top_bottom (to_string (TOP));

				elsif bottom_enabled then
					top_bottom (to_string (BOTTOM));
							
				end if;
			end if;
		end draw_side_name;



		use et_display;
		use et_display.board;

		
		procedure silkscreen is begin
			pos := to_vector (cms.silk_screen.position);
			
			draw_text (
				content		=> to_content (to_string (cms.silk_screen.content)),
				size		=> to_distance (cms.silk_screen.size),
				font		=> font_placeholders,
				anchor		=> add (pos, tb_position.place),
				origin		=> false,
				rotation	=> 0.0,
				alignment	=> (ALIGN_LEFT, ALIGN_BOTTOM));
		end silkscreen;
			

		procedure assy_doc is begin
			pos := to_vector (cms.assy_doc.position);
			
			draw_text (
				content		=> to_content (to_string (cms.assy_doc.content)),
				size		=> to_distance (cms.assy_doc.size),
				font		=> font_placeholders,
				anchor		=> add (pos, tb_position.place),
				origin		=> false,
				rotation	=> 0.0,
				alignment	=> (ALIGN_LEFT, ALIGN_BOTTOM));
		end assy_doc;

		
		procedure keepout is begin
			pos := to_vector (cms.keepout.position);

			draw_text (
				content		=> to_content (to_string (cms.keepout.content)),
				size		=> to_distance (cms.keepout.size),
				font		=> font_placeholders,
				anchor		=> add (pos, tb_position.place),
				origin		=> false,
				rotation	=> 0.0,
				alignment	=> (ALIGN_LEFT, ALIGN_BOTTOM));
		end keepout;

		
		procedure stop_mask is begin
			pos := to_vector (cms.stop_mask.position);

			draw_text (
				content		=> to_content (to_string (cms.stop_mask.content)),
				size		=> to_distance (cms.stop_mask.size),
				font		=> font_placeholders,
				anchor		=> add (pos, tb_position.place),
				origin		=> false,
				rotation	=> 0.0,
				alignment	=> (ALIGN_LEFT, ALIGN_BOTTOM));
		end stop_mask;

		
		procedure stencil is begin
			pos := to_vector (cms.stencil.position);

			draw_text (
				content		=> to_content (to_string (cms.stencil.content)),
				size		=> to_distance (cms.stencil.size),
				font		=> font_placeholders,
				anchor		=> add (pos, tb_position.place),
				origin		=> false,
				rotation	=> 0.0,
				alignment	=> (ALIGN_LEFT, ALIGN_BOTTOM));
		end stencil;


		procedure plated_millings is begin
			pos := to_vector (cms.plated_millings.position);

			draw_text (
				content		=> to_content (to_string (cms.plated_millings.content)),
				size		=> to_distance (cms.plated_millings.size),
				font		=> font_placeholders,
				anchor		=> add (pos, tb_position.place),
				origin		=> false,
				rotation	=> 0.0,
				alignment	=> (ALIGN_LEFT, ALIGN_BOTTOM));
		end plated_millings;


		procedure outline is begin
			pos := to_vector (cms.pcb_outline.position);

			draw_text (
				content		=> to_content (to_string (cms.pcb_outline.content)),
				size		=> to_distance (cms.pcb_outline.size),
				font		=> font_placeholders,
				anchor		=> add (pos, tb_position.place),
				origin		=> false,
				rotation	=> 0.0,
				alignment	=> (ALIGN_LEFT, ALIGN_BOTTOM));
		end outline;


		procedure route_restrict is begin
			pos := to_vector (cms.route_restrict.position);

			draw_text (
				content		=> to_content (to_string (cms.route_restrict.content)),
				size		=> to_distance (cms.route_restrict.size),
				font		=> font_placeholders,
				anchor		=> add (pos, tb_position.place),
				origin		=> false,
				rotation	=> 0.0,
				alignment	=> (ALIGN_LEFT, ALIGN_BOTTOM));
		end route_restrict;
		

		procedure via_restrict is begin
			pos := to_vector (cms.via_restrict.position);

			draw_text (
				content		=> to_content (to_string (cms.via_restrict.content)),
				size		=> to_distance (cms.via_restrict.size),
				font		=> font_placeholders,
				anchor		=> add (pos, tb_position.place),
				origin		=> false,
				rotation	=> 0.0,
				alignment	=> (ALIGN_LEFT, ALIGN_BOTTOM));
		end via_restrict;


		procedure conductor_layers is begin
			-- Draw something like "SGNL_LYR:"
			pos := to_vector (cms.signal_layer.position);

			draw_text (
				content		=> to_content (to_string (cms.signal_layer.content)),
				size		=> to_distance (cms.signal_layer.size),
				font		=> font_placeholders,
				anchor		=> add (pos, tb_position.place),
				origin		=> false,
				rotation	=> 0.0,
				alignment	=> (ALIGN_LEFT, ALIGN_BOTTOM));
			

			-- Draw the layer numbers like: "1,2,5..32"
			pos := to_vector (aps.signal_layer.position);
				
			draw_text (
				content		=> to_content (enabled_conductor_layers),
				size		=> to_distance (aps.signal_layer.size),
				font		=> font_placeholders,
				anchor		=> add (pos, tb_position.place),
				origin		=> false,
				rotation	=> 0.0,
				alignment	=> (ALIGN_LEFT, ALIGN_BOTTOM));
		end conductor_layers;

		
		
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
			plated_millings;
		end if;


		-- outline
		if outline_enabled then
			outline;
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
			route_restrict;
		end if;

		
		-- via restrict
		if via_restrict_enabled then
			via_restrict;
		end if;
		

		-- conductor layers
		if conductors_enabled then
			conductor_layers;
		end if;

		
		draw_side_name;  -- draws something like "SIDE: TOP"
		
	end draw_cam_markers;
	
	
begin
	-- put_line ("draw_drawing_frame (board)");

	set_color_frame;

	-- FRAME:
	draw_frame (frame.frame);


	
	-- TITLE BLOCK:

	-- Compute the position of the title block
	-- relative to the lower-left corner of the drawing frame.
	-- The initial position of the title-block is specified
	-- the template file (*.frb).
	-- The final position of the title-block in the drawing 
	-- is a result of adding the frame position to the
	-- initial title-block position.
	-- So when the frame position is changed, then the title-block
	-- moves along with the frame.
	tb_position.place := to_vector (add (
		right	=> frame.frame.position, -- the lower-left corner of the frame
		left	=> title_block.position)); -- the title-block position
	

	-- Draw the lines the title block is composed of:
	draw_title_block_lines;	


	-- Draw schematic and board common placeholders like 
	-- project name, module file name, active assembly variant:
	draw_common_placeholders (
		placeholders			=> title_block.placeholders_common,
		title_block_position	=> tb_position);


	
	-- Draw static texts like "sheet", "description", "company", ... :
	draw_static_texts (
		texts					=> title_block.static_texts,
		title_block_position	=> tb_position);



	-- Draw meta information like company name, revision, persons
	-- who have drawn, checked and approved the drawing ... :
	draw_basic_meta_information (
		meta					=> et_board_ops.get_basic_meta_information (active_module),
		placeholders			=> type_placeholders_basic (title_block.placeholders_additional),
		title_block_position	=> tb_position);

	
	draw_cam_markers;
	
end draw_drawing_frame;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
