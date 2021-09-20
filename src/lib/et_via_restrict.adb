------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                            VIA RESTRICT                                  --
--                                                                          --
--                              B o d y                                     --
--                                                                          --
--         Copyright (C) 2017 - 2021 Mario Blunk, Blunk electronic          --
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
--   to do:


with ada.strings;	 			use ada.strings;

package body et_via_restrict is

	procedure line_via_restrict_properties (
		face			: in type_face;
		cursor			: in pac_via_restrict_lines.cursor;
		log_threshold 	: in type_log_level) 
	is
		use pac_via_restrict_lines;
		line : type_via_restrict_line;
	begin
		line := element (cursor);
		log (text => "via restrict line face" & to_string (face) & space
			 & to_string (type_line (line)), level => log_threshold);
	end line_via_restrict_properties;

	
	procedure arc_via_restrict_properties (
		face			: in type_face;
		cursor			: in pac_via_restrict_arcs.cursor;
		log_threshold 	: in type_log_level)
	is
		use pac_via_restrict_arcs;
		arc : type_via_restrict_arc;
	begin
		arc := element (cursor);
		log (text => "via restrict arc face" & to_string (face) & space 
			 & to_string (type_arc (arc)), level => log_threshold);
	end arc_via_restrict_properties;

	
	-- CS procedure circle_via_restrict_properties


	
end et_via_restrict;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
