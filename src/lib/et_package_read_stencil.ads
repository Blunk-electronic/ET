------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                        PACKAGE READ / STENCIL                            --
--                                                                          --
--                               S p e c                                    --
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
-- DESCRIPTION:
-- 
-- This is about lines, arcs and circles in the stencil.
--
--   do do:
--
--

with et_string_processing;				use et_string_processing;
with et_package_model;					use et_package_model;
with et_pcb_sides;						use et_pcb_sides;
with et_logging;						use et_logging;


package et_package_read_stencil is



	procedure read_stencil_line (
		line : in type_fields_of_line);
	
	
	procedure read_stencil_arc (
		line : in type_fields_of_line);


	procedure read_stencil_circle (
		line : in type_fields_of_line);
	


	procedure insert_stencil_line (
		packge			: in type_package_model_access;
		face			: in type_face;
		log_threshold	: in type_log_level);


	procedure insert_stencil_arc (
		packge			: in type_package_model_access;
		face			: in type_face;
		log_threshold	: in type_log_level);


	procedure insert_stencil_circle (
		packge			: in type_package_model_access;
		face			: in type_face;
		log_threshold	: in type_log_level);
	

	procedure insert_stencil_zone (
		packge			: in type_package_model_access;
		face			: in type_face;
		log_threshold	: in type_log_level);


	
end et_package_read_stencil;
