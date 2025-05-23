------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                       BOARD OPERATIONS / TEXT                            --
--                                                                          --
--                               B o d y                                    --
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

--   For correct displaying set tab with in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with et_assembly_variants;
with et_conductor_text.boards;


package body et_board_ops.text is

	
	-- Maps from the meaning of a text to its actutal content.
	function to_placeholder_content (
		module_cursor	: in pac_generic_modules.cursor;
		meaning 		: in type_text_meaning)
		return et_text.pac_text_content.bounded_string 
	is
		m : type_generic_module renames element (module_cursor);
		
		use et_text;

		use et_meta;
		meta : constant et_meta.type_board := m.meta.board;

		use et_assembly_variants;
		use pac_assembly_variant_name;
		variant : constant pac_assembly_variant_name.bounded_string := m.active_variant;

		result : pac_text_content.bounded_string;

		use et_pcb;
	begin
		case meaning is
			when COMPANY			=> result := to_content (to_string (meta.company));
			when CUSTOMER			=> result := to_content (to_string (meta.customer));
			when PARTCODE			=> result := to_content (to_string (meta.partcode));
			when DRAWING_NUMBER		=> result := to_content (to_string (meta.drawing_number));
			when ASSEMBLY_VARIANT	=> result := to_content (to_string (variant));
			when PROJECT			=> result := to_content ("not assigned"); -- CS
			when MODULE				=> result := to_content (to_string (key (module_cursor)));
			when REVISION			=> result := to_content (to_string (meta.revision));
		end case;
		
		return result;
	end to_placeholder_content;
	

end et_board_ops.text;
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
