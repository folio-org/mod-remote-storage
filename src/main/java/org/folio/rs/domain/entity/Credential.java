package org.folio.rs.domain.entity;

import java.util.UUID;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Table(name = "CREDENTIALS")
@Data
@Entity
@NoArgsConstructor
@AllArgsConstructor(staticName = "of")
public class Credential {
  @Id
  private UUID id;
  @Column(name = "username")
  private String username;
  @Column(name = "password")
  private String password;
}
