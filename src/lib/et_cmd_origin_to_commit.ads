------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                       COMMAND ORIGIN TO COMMIT                           --
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

with et_string_processing;		use et_string_processing;
with et_cmd_sts;				use et_cmd_sts;


package et_cmd_origin_to_commit is

	-- Commands can be given via the console (inside the GUI)
	-- or via script.
	-- If a command origin is the console, then
	-- the design state before and after the execution
	-- must be committed.
	-- Committing the design state allows undo and redo
	-- operations.
	-- Commands given via script are not committed as there is
	-- no reason to do that.

	type type_commit_design is (
		DO_COMMIT,
		NO_COMMIT);


	-- Maps from the origin of the given command
	-- to whether to commit the design state or not.
	-- Returns DO_COMMIT if the command origin is console.
	-- Returns NO_COMMIT if the origin is script.
	function to_commit_design (
		cmd : in type_single_cmd)
		return type_commit_design;
		

	
end et_cmd_origin_to_commit;

