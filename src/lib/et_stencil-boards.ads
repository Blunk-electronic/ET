------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                          STENCIL BOARDS                                  --
--                                                                          --
--                              S p e c                                     --
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

package et_stencil.boards is

	-- NOTE: Probably not reasonable and a waste of resources.
	-- DRC should output warning if texts in stencil detected.

	
	type type_stencil_text is new type_text_fab_with_content with record
		vectors	: pac_vector_text_lines.list;
	end record;

	package pac_stencil_texts is new doubly_linked_lists (type_stencil_text);
	

	type type_stencil 
		is new et_stencil.type_stencil with 
	record
		texts		: pac_stencil_texts.list;
	end record;


	-- Because stencil is about two sides of the board this composite is required:
	type type_stencil_both_sides is record
		top		: type_stencil;
		bottom	: type_stencil;
	end record;

	
end et_stencil.boards;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
