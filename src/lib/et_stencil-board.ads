------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                     STENCIL / SOLDER PASTE MASK / BOARD                  --
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
--   to do:
--


with et_pcb_sides;						use et_pcb_sides;
with et_pcb_placeholders;				use et_pcb_placeholders;


package et_stencil.board is



	type type_stencil_both_sides is record
		top		: type_stencil;
		bottom	: type_stencil;
	end record;

	
	

	procedure add_line (
		stencil	: in out type_stencil_both_sides;
		line		: in type_stencil_line;
		face		: in type_face);

		
	procedure add_arc (
		stencil	: in out type_stencil_both_sides;
		arc			: in type_stencil_arc;
		face		: in type_face);

		
	procedure add_circle (
		stencil	: in out type_stencil_both_sides;
		circle		: in type_stencil_circle;
		face		: in type_face);


	
end et_stencil.board;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
