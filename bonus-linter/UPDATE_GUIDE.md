# 🔄 Guide de Mise à Jour de l'Extension

## Changements effectués (v1.0.0 → v1.0.1)

### ✅ Modifications
- Suppression du thème d'icônes personnalisé (Seti gère déjà les .rat)
- Mise à jour du README avec installation marketplace
- Ajout des liens repository dans package.json
- Mise à jour du CHANGELOG

### 📦 Pour publier la mise à jour

#### 1. Incrémenter la version
```bash
npm version patch
# Cela passe automatiquement de 1.0.0 à 1.0.1
```

#### 2. Recompiler
```bash
npm run compile
```

#### 3. Créer le nouveau VSIX
```bash
./create-vsix.sh
```

#### 4. Publier sur le marketplace

**Option A : Upload manuel**
1. Aller sur https://marketplace.visualstudio.com/manage
2. Cliquer sur votre extension "Ratatouille Language Support"
3. Cliquer sur "Update"
4. Drag & drop le nouveau fichier `ratatouille-language-support-1.0.1.vsix`

**Option B : Publier directement**
```bash
nvm use 20
vsce publish
```

#### 5. Vérifier
- Les utilisateurs recevront une notification de mise à jour
- La nouvelle version apparaîtra sur le marketplace
- L'historique des versions sera mis à jour

---

## 🎯 Notes pour les prochaines mises à jour

### Types de versions

- **Patch** (1.0.0 → 1.0.1) : Corrections de bugs
  ```bash
  npm version patch
  ```

- **Minor** (1.0.0 → 1.1.0) : Nouvelles fonctionnalités
  ```bash
  npm version minor
  ```

- **Major** (1.0.0 → 2.0.0) : Changements incompatibles
  ```bash
  npm version major
  ```

### Checklist avant publication

- [ ] Code compilé sans erreurs
- [ ] Testé en mode développement (F5)
- [ ] CHANGELOG.md mis à jour
- [ ] Version incrémentée
- [ ] README à jour si nécessaire

---

**Temps estimé pour une mise à jour** : 5 minutes ⚡
