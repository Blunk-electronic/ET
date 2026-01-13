------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                        PACKAGE WRITE / KEEPOUT                           --
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

with et_keepout;						use et_keepout;
with et_keepout.packages;				use et_keepout.packages;

with et_general_rw;						use et_general_rw;
with et_board_geometry;					use et_board_geometry;



package body et_package_write_keepout is


	use pac_file_rw;
	
	use pac_keepout_zones;
	use pac_keepout_cutouts;

	

	procedure write_keepout (
		packge			: in type_package_model;
		log_threshold	: in type_log_level) 
	is


		procedure write_polygon (cursor : in pac_keepout_zones.cursor) is 
		begin
			section_mark (section_zone, HEADER);
			section_mark (section_contours, HEADER);
			write_polygon_segments (element (cursor));
			section_mark (section_contours, FOOTER);
			section_mark (section_zone, FOOTER);
		end write_polygon;


		procedure write_cutout (cursor : in pac_keepout_cutouts.cursor) is 
			use pac_keepout_cutouts;
		begin
			section_mark (section_cutout_zone, HEADER);
			section_mark (section_contours, HEADER);
			write_polygon_segments (element (cursor));
			section_mark (section_contours, FOOTER);
			section_mark (section_cutout_zone, FOOTER);
		end;

		
		
	begin
		log (text => "write keepout", level => log_threshold);

		section_mark (section_keepout, HEADER);

		-- top
		section_mark (section_top, HEADER);
		iterate (packge.keepout.top.zones, write_polygon'access);
		iterate (packge.keepout.top.cutouts, write_cutout'access);
		section_mark (section_top, FOOTER);
		
		-- bottom
		section_mark (section_bottom, HEADER);
		iterate (packge.keepout.bottom.zones, write_polygon'access);			
		iterate (packge.keepout.bottom.cutouts, write_cutout'access);
		section_mark (section_bottom, FOOTER);

		section_mark (section_keepout, FOOTER);				

	end write_keepout;

	
end et_package_write_keepout;
