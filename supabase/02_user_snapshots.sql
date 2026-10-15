-- BudgetForge v5 compatibility bridge. Run in Supabase SQL Editor.
-- Adds a private snapshot per verified user. Does not migrate the prior 15 tables.
BEGIN;
CREATE TABLE IF NOT EXISTS public.budgetforge_snapshots (
  user_id uuid PRIMARY KEY DEFAULT auth.uid() REFERENCES auth.users(id) ON DELETE CASCADE,
  revision bigint NOT NULL DEFAULT 1 CHECK(revision >= 1),
  snapshot_b64 text NOT NULL,
  updated_at timestamptz NOT NULL DEFAULT now()
);
ALTER TABLE public.budgetforge_snapshots ENABLE ROW LEVEL SECURITY;
REVOKE ALL ON public.budgetforge_snapshots FROM anon;
GRANT SELECT, INSERT, UPDATE ON public.budgetforge_snapshots TO authenticated;
DROP POLICY IF EXISTS budgetforge_snapshot_user_only ON public.budgetforge_snapshots;
CREATE POLICY budgetforge_snapshot_user_only ON public.budgetforge_snapshots
  FOR ALL TO authenticated
  USING ((SELECT auth.uid()) = user_id)
  WITH CHECK ((SELECT auth.uid()) = user_id);
COMMIT;
