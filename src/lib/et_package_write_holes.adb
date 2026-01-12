------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                       PACKAGE WRITE / HOLES                              --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2026                                                --
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
--
-- To Do:
-- - clean up, use renames



with ada.text_io;				use ada.text_io;
-- with ada.characters.handling;	use ada.characters.handling;
-- with ada.strings; 				use ada.strings;

with et_keywords;						use et_keywords;
with et_section_headers;				use et_section_headers;
with et_package_sections;				use et_package_sections;

with et_board_holes;					use et_board_holes;

with et_board_geometry;					use et_board_geometry;
with et_general_rw;						use et_general_rw;
with et_board_write;					use et_board_write;


package body et_package_write_holes is

	use pac_geometry_2;
	use pac_file_rw;
	
	use pac_holes;

	

	procedure write_holes (
		packge			: in type_package_model;
		log_threshold	: in type_log_level) 
	is

		procedure query_hole (c : in pac_holes.cursor) is begin
			section_mark (section_hole, HEADER);		
			write_polygon_segments (element (c));		
			section_mark (section_hole, FOOTER);		
		end query_hole;

		
	begin
		log (text => "write holes", level => log_threshold);

		if not is_empty (packge.holes) then
			
			section_mark (section_pcb_contours, HEADER);		
			packge.holes.iterate (query_hole'access);				
			section_mark (section_pcb_contours, FOOTER);
			
		end if;
	end write_holes;

	
	
end et_package_write_holes;
