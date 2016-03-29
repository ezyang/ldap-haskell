#include <ldap.h>

static int sasl_trivial_interact (LDAP *ld, unsigned flags, void *defaults,
                                  void *sasl_interact)
{
    (void)ld;
    (void)flags;
    (void)defaults;
    (void)sasl_interact;
    return LDAP_SUCCESS;
}

int sasl_trivial_external (LDAP *ld)
{
    return ldap_sasl_interactive_bind_s (ld, NULL, "EXTERNAL", NULL, NULL,
                                         LDAP_SASL_QUIET,
                                         sasl_trivial_interact, NULL);
}
