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

with ada.strings.bounded; 	use ada.strings.bounded;

with et_frames;				use et_frames;

package et_pcb_sheet is

-- 	-- $ET_FRAMES/drawing_frame_version_1.frb
-- 	template_file_extension : constant string := "frb";
-- 	
-- 	package pac_template_name is new generic_bounded_length (template_file_name_length_max);
-- 
-- 	frame_template_name_dummy : constant pac_template_name.bounded_string := 
-- 		pac_template_name.to_bounded_string (template_file_name_dummy);
-- 	
-- 	function to_string (name : in pac_template_name.bounded_string) return string;
-- 	function to_template_name (name : in string) return pac_template_name.bounded_string;
-- 
-- 
-- 	type type_text_placeholders_additional is record
-- 		silk_screen, assy_doc, 
-- 
-- 		keepout, plated_millings, pcb_outline, 
-- 
-- 		route_restrict, via_restrict, signal_layer	: type_placeholder;
-- 		-- CS add more
-- 	end record;
-- 	
-- 	type type_title_block is new et_frames.type_title_block with record
-- 		additional_placeholders : type_text_placeholders_additional;
-- 	end record;
-- 
-- 
-- 	-- This is the drawing frame used in a pcb layout:
-- 	type type_frame is new et_frames.type_frame with record
-- 		template	: pac_template_name.bounded_string := frame_template_name_dummy;
-- 			-- like $ET_FRAMES/drawing_frame_A3_landscape.frm
-- 
-- 		title_block : type_title_block;
-- 	end record;

	procedure dummy;
		
end et_pcb_sheet;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
