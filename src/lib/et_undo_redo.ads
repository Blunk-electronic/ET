------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                       UNDO / REDO OPERATIONS                             --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2024                                                -- 
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
-- DESCRIPTION:
-- 

with ada.strings.bounded;       	use ada.strings.bounded;

with et_project.modules;			use et_project.modules;

with et_modes.schematic;
with et_modes.board;
with et_commit;						use et_commit;
with et_logging;					use et_logging;


package et_undo_redo is

	use pac_generic_modules;

	-- Puts a fragment of the design on the undo-stacks of
	-- the current active module.
	-- The fragment can be nets, silkscreen, board contours, meta info, ...
	-- The affected fragment to be committed depends on the
	-- given verb and noun.
	-- This commit procedure must be called before and after a 
	-- schematic operation. For this reason we speak about
	-- PRE- and POST-commits (see package et_commit). A complete
	-- commit consists of a PRE- and a POST-commit.
	-- So this procedure requires to be called twice.
	procedure commit (
		stage	: in type_commit_stage;
		verb	: in et_modes.schematic.type_verb;
		noun	: in et_modes.schematic.type_noun;
		lth		: in type_log_level);


	-- Likewise as the procedure above.
	-- This commit procedure must be called before and after a 
	-- schematic operation:
	procedure commit (
		stage	: in type_commit_stage;
		verb	: in et_modes.board.type_verb;
		noun	: in et_modes.board.type_noun;
		lth		: in type_log_level);



	
	undo_message_length_max : constant positive := 50;
	package pac_undo_message is new generic_bounded_length (undo_message_length_max);

	
	-- Restores the design state (both in schematic and board)
	-- to the state BEFORE the latest commit.
	-- Moves the latest PRE- and POST-commit to one of the the redo-stacks.
	-- Via the argument "message" the procedure returns some helpful information
	-- to be displayed in the GUI:
	procedure undo (
		message	: in out pac_undo_message.bounded_string;
		lth		: in type_log_level);
	

	redo_message_length_max : constant positive := 50;
	package pac_redo_message is new generic_bounded_length (redo_message_length_max);

	
	-- Redoes the the latest undo-operation:
	-- Moves the latest PRE- and POST-commit back to one of the the do-stacks.
	-- Via the argument "message" the procedure returns some helpful information
	-- to be displayed in the GUI:
	procedure redo  (
		message	: in out pac_redo_message.bounded_string;
		lth		: in type_log_level);
	
	
end et_undo_redo;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
