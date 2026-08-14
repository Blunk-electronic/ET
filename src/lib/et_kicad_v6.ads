------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              KICAD V6                                    --
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
-- DESCRIPTION:
--
--   Root package of the KiCad version 6 (and later, S-expression based)
--   import. Unlike et_kicad (which imports KiCad version 4/5 flat-text
--   projects), this package tree is deliberately independent of et_kicad --
--   the file formats do not share a lexical structure, so nothing about
--   the v4 parser is reused here. See et_kicad_v6.sexp for the generic
--   S-expression reader and et_kicad_v6.schematic for the schematic model
--   and hierarchical project loader.
--
--   history of changes:
--

with ada.strings.bounded;			use ada.strings.bounded;
with ada.containers.doubly_linked_lists;

with et_bounded_string_helpers;
with et_logging;					use et_logging;

package et_kicad_v6 is


	-- A KiCad UUID as it appears verbatim in the source file, e.g.
	-- "20011966-1a47-4c68-7951-4279a300d86b". Canonical form is 36
	-- characters -- the extra margin is defensive:
	uuid_length_max : constant natural := 40;

	package pac_uuid is new generic_bounded_length (uuid_length_max);
	type type_uuid is new pac_uuid.bounded_string;

	function to_string is new et_bounded_string_helpers.to_string   (pac_uuid, from_type => type_uuid);
	function to_uuid    is new et_bounded_string_helpers.from_string (pac_uuid, to_type   => type_uuid);


	-- A sequence of sheet UUIDs identifying one particular instance
	-- of a sheet within the hierarchy, root first. This is how KiCad
	-- v6 addresses "this occurrence of this sheet" (via the
	-- "instances"/"path" and "sheet_instances"/"path" mechanisms) --
	-- see et_kicad_v6.schematic for where this is used:
	package pac_uuid_path is new ada.containers.doubly_linked_lists (type_uuid);

	-- Renders a uuid path the same way KiCad writes it in a "path"
	-- property, e.g. "/uuid1/uuid2":
	function to_string (path : in pac_uuid_path.list) return string;

	-- Splits a "/uuid1/uuid2" style path (as read from a "path"
	-- property) into a uuid path. A leading slash is expected and
	-- ignored; an empty string yields an empty list:
	function to_uuid_path (text : in string) return pac_uuid_path.list;


	-- Every dispatch loop throughout et_kicad_v6.schematic that
	-- branches on the "tag" (head) of a parsed s-expression list
	-- calls this instead of silently ignoring anything it does not
	-- recognize -- see the "Error handling" section of the project
	-- plan. deferred => true means the key is a documented, modeled-
	-- but-not-yet-populated stub (e.g. sheet pins, bus aliases) and
	-- is logged as a NOTE; deferred => false means the key was not
	-- anticipated at all and is logged as a WARNING:
	procedure log_unknown_key (
		context			: in string; -- e.g. "sheet", "symbol", "wire"
		key				: in string; -- the unrecognized head/tag
		log_threshold	: in type_log_level;
		deferred		: in boolean := false);


end et_kicad_v6;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
