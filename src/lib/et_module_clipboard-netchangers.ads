------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                   MODULE CLIPBOARD NETCHANGERS                           --
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
--  To Do:
--
--


with et_module;
with et_generic_modules;			use et_generic_modules;
with et_schematic_coordinates;		use et_schematic_coordinates;

with et_netchangers;				use et_netchangers;
with et_logging;					use et_logging;



package et_module_clipboard.netchangers is

-- COPY:

	-- This procedure copies a given netchanger
	-- to the clipboard:
	procedure copy_netchanger_to_clipboard (
		netchanger_cursor	: in pac_netchangers.cursor;
		log_threshold		: in type_log_level);




	-- This procedure copies selected netchangers to
	-- the clipboard:
	procedure copy_selected_netchangers_to_clipboard (
		module_cursor	: in pac_generic_modules.cursor;
		-- CS sheet : in type_sheet; -- to speed up the process ?
		log_threshold	: in type_log_level);



-- PASTE:
	
	-- This procedure copies netchangers from the clipboard
	-- to the given module. The netchangers will be placed
	-- by the given offset:
	procedure paste_netchangers_from_clipboard (
		module_cursor	: in pac_generic_modules.cursor;
		offset			: in type_object_position_relative;
		log_threshold	: in type_log_level);




end et_module_clipboard.netchangers;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
