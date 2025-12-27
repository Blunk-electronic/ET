------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                   ASSEMBLY DOCUMENTATION / BOARD                         --
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
with et_pcb_placeholders.non_conductor;	use et_pcb_placeholders.non_conductor;


package et_assy_doc.board is



	type type_assy_doc_board is new type_assy_doc with record
		-- Placeholders for revision, board name, misc ... :
		placeholders : pac_text_placeholders.list;
	end record;


	-- Because assembly documentation is about two 
	-- sides of the board this composite is required:	
	type type_assy_doc_both_sides is record
		top 	: type_assy_doc_board;
		bottom	: type_assy_doc_board;
	end record;



	procedure add_line (
		assy_doc	: in out type_assy_doc_both_sides;
		line		: in type_doc_line;
		face		: in type_face);

		
	procedure add_arc (
		assy_doc	: in out type_assy_doc_both_sides;
		arc			: in type_doc_arc;
		face		: in type_face);

		
	procedure add_circle (
		assy_doc	: in out type_assy_doc_both_sides;
		circle		: in type_doc_circle;
		face		: in type_face);



	-- CS procedure add_zone, add_text, add_placeholder
	
	
end et_assy_doc.board;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
