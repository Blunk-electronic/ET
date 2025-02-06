------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                     		      RIPUP                                     --
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



package body et_ripup is


	function to_string (
		mode : in type_ripup_mode)
		return string
	is begin
		return "ripup mode: " & type_ripup_mode'image (mode);
	end to_string;

	

	procedure reset_ripup_mode is begin
		ripup_mode := SINGLE_SEGMENT;
	end reset_ripup_mode;

	

	procedure next_ripup_mode is
		i : constant natural := type_ripup_mode'pos (ripup_mode);
		-- i points now to the current ripup mode

		-- get the index of the last available ripup mode:
		max : constant natural := type_ripup_mode'pos (type_ripup_mode'last);
	begin
		if i < max then
			-- jump to next mode
			ripup_mode := type_ripup_mode'succ (type_ripup_mode'val (i));
		else 
			-- After the last mode, jump back to the first mode:
			ripup_mode := type_ripup_mode'first;
		end if;
	end next_ripup_mode;

	
	
end et_ripup;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
