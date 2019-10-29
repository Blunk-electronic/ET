------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              PCB SHEET                                   --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2019 Mario Blunk, Blunk electronic                 --
--                                                                          --
--    This program is free software: you can redistribute it and/or modify  --
--    it under the terms of the GNU General Public License as published by  --
--    the Free Software Foundation, either version 3 of the License, or     --
--    (at your option) any later version.                                   --
--                                                                          --
--    This program is distributed in the hope that it will be useful,       --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of        --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         --
--    GNU General Public License for more details.                          --
--                                                                          --
--    You should have received a copy of the GNU General Public License     --
--    along with this program.  If not, see <http://www.gnu.org/licenses/>. --
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
--   ToDo: 

with et_geometry;
with et_pcb_coordinates;	use et_pcb_coordinates;
with et_packages;
with et_text;				use et_text;
with et_frames;				use et_frames;

package et_pcb_sheet is

	use et_pcb_coordinates.geometry;
	use type_text_content;

	package pac_shapes is new et_geometry.shapes_2d (
		geometry	=> et_pcb_coordinates.geometry);
	
	package pac_frames is new et_frames.frames (
		shapes	=> pac_shapes,
		text	=> et_packages.pac_text);
	
	use pac_frames;
	
	procedure dummy;


	
-- FRAMES

	type type_text_placeholders is record
		silk_screen, assy_doc, 

		keepout, plated_millings, pcb_outline, 

		route_restrict, via_restrict, signal_layer	: type_text_placeholder;
		-- CS add more
	end record;
	
	-- The title block of the single layout sheet requires additional placeholders:
	type type_title_block is new pac_frames.type_title_block with record
		additional_placeholders : type_text_placeholders;
	end record;

	-- This is the drawing frame used in a pcb layout:
	type type_frame is new pac_frames.type_frame with record
		title_block : type_title_block;
	end record;

		
end et_pcb_sheet;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
