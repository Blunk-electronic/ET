------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                         TERMINAL THROUGH HOLE                            --
--                                                                          --
--                              B o d y                                     --
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
-- to do:
--





package body et_terminal_tht is



	procedure log_plated_millings (
		millings 		: in type_contour;
		log_threshold	: in type_log_level)
		is
-- 		use type_pcb_contour_lines;
-- 		use type_pcb_contour_arcs;
-- 		use type_pcb_contour_circles;
-- 		
-- 		procedure line (cursor : in type_pcb_contour_lines.cursor) is begin
-- 			line_pcb_contour_properties (cursor, log_threshold);
-- 		end;
-- 
-- 		procedure arc (cursor : in type_pcb_contour_arcs.cursor) is begin
-- 			arc_pcb_contour_properties (cursor, log_threshold);
-- 		end;
-- 
-- 		procedure circle (cursor : in type_pcb_contour_circles.cursor) is begin
-- 			circle_pcb_contour_properties (cursor, log_threshold);
-- 		end;
		
	begin -- log_plated_millings
		null;
-- CS
-- 		iterate (millings.lines, line'access);
-- 		iterate (millings.arcs, arc'access);
-- 		iterate (millings.circles, circle'access);
	end log_plated_millings;

	

	
	
end et_terminal_tht;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
