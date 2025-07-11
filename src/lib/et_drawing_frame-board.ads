------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          DRAWING FRAME BOARD                             --
--                                                                          --
--                               S p e c                                    --
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

with ada.strings.bounded; 		use ada.strings.bounded;
with ada.strings.maps;			use ada.strings.maps;
with ada.containers; 			use ada.containers;
with ada.containers.doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.directories;			use ada.directories;

with et_axes;					use et_axes;
with et_text;					use et_text;
with et_sheets;					use et_sheets;
with et_fonts;					use et_fonts;


package et_drawing_frame.board is

	template_pcb_extension			: constant string := "frb"; -- $ET_FRAMES/drawing_frame_version_1.frb


	template_pcb_default : constant pac_template_name.bounded_string := 
		pac_template_name.to_bounded_string (
			compose (
				name		=> template_file_name_dummy,
				extension	=> template_pcb_extension)
				);




-- TEXT PLACEHOLDERS AND TITLE BLOCKS
	

	
	-- CAM markers are required for CAM output and visualization.
	-- They are texts in the title block that indicate what it is about.
	-- Depending on the structures being displayed or exported, they are displayed or not.
	-- Their content may be specified by the operator in the respective sections in the frame file (*.frb).
	-- The content specified in the frame file overrides the default content. 
	-- If they are not specified by the frame file, default position, size and content is used (see below).
	type type_cam_marker is new type_static_text with null record;

	type type_cam_markers is record
		face			: type_cam_marker := (content => to_content ("FACE:"), others => <>);
		silk_screen		: type_cam_marker := (content => to_content ("SILKSCREEN"), others => <>);
		assy_doc		: type_cam_marker := (content => to_content ("ASSEMBLY"), others => <>);
		keepout			: type_cam_marker := (content => to_content ("KEEPOUT"), others => <>);
		plated_millings	: type_cam_marker := (content => to_content ("PLTD_MILLGS"), others => <>); 
		pcb_outline 	: type_cam_marker := (content => to_content ("OUTLINE"), others => <>);
		route_restrict	: type_cam_marker := (content => to_content ("ROUTE_RESTRICT"), others => <>);
		via_restrict	: type_cam_marker := (content => to_content ("VIA_RESTRICT"), others => <>);		
		signal_layer	: type_cam_marker := (content => to_content ("SGNL_LYR:"), others => <>);
		stencil			: type_cam_marker := (content => to_content ("STENCIL"), others => <>);		
		stop_mask		: type_cam_marker := (content => to_content ("STOP_MASK"), others => <>);
	end record;


	
	type type_placeholders_pcb is new type_placeholders_basic with record
		face			: type_placeholder; -- to be filled with the word "TOP" or "BOTTOM"
		signal_layer	: type_placeholder; -- to be filled with the signal layer id like 1,2,3, 8..16
	end record;


	
	type type_title_block_pcb is new type_title_block with record
		placeholders_additional	: type_placeholders_pcb;
		cam_markers				: type_cam_markers;
	end record;



	
	type type_frame_pcb_pre is new type_frame_general with record
		title_block_pcb : type_title_block_pcb;
	end record;
	-- CS: find a more reasonable type name.


	
	procedure apply_defaults_board (frame : in out type_frame_pcb_pre);

	
	
	-- Generates a default frame for the given domain:
	-- function make_default_frame (domain : in type_domain) 
	-- 	return type_frame;

	function make_default_frame_pcb
		return type_frame_pcb_pre;

	
	
-- THE FINAL FRAME IN A PCB DRAWING
	
	-- This is the drawing frame used in a pcb layout:
	type type_frame_pcb is record
		template	: pac_template_name.bounded_string := template_pcb_default;
			-- like $ET_FRAMES/drawing_frame_A3_landscape.frb

		--frame		: type_frame (DOMAIN_PCB) := make_default_frame (DOMAIN_PCB);
		frame		: type_frame_pcb_pre := make_default_frame_pcb;
	end record;

	
end et_drawing_frame.board;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
